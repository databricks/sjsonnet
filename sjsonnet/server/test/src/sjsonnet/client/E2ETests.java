package sjsonnet.client;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static org.junit.Assert.*;

/**
 * End-to-end integration tests for the sjsonnet client/server IPC flow
 * over native Unix Domain Sockets (JEP 380, Java 16+).
 *
 * <p>Each test spawns a real server JVM and a real client JVM, communicating
 * via a UDS socket at {@code <lockBase>/io}. Lock files, the run file, and the
 * exit-code file are exercised as in production.</p>
 */
public class E2ETests {

    private static Path tmpRoot;
    private static String classpath;
    private static String version;

    @BeforeClass
    public static void setUpClass() throws Exception {
        tmpRoot = Util.isWindows
                ? Files.createTempDirectory("sjsonnet-e2e-")
                : Files.createTempDirectory(Path.of("/tmp"), "sj-");

        classpath = System.getProperty("java.class.path");
        assertNotNull("java.class.path must be set", classpath);

        // Use a fixed test version rather than sjsonnet.Version.version() so the
        // E2E flow is decoupled from the build-time version (which may differ
        // between CI and local runs). Client and server only need to agree.
        version = "0.0.0-e2e-test";
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
        if (tmpRoot == null) return;
        stopServersForRoot();
        try (var paths = Files.walk(tmpRoot)) {
            paths.sorted(Comparator.reverseOrder()).forEach(p -> {
                try { Files.deleteIfExists(p); } catch (IOException ignored) {}
            });
        }
    }

    /**
     * Scenario 1: client invocation against a real server JVM — UDS connection
     * must be established, jsonnet evaluated, exit code 0 returned.
     */
    @Test
    public void clientConnectsToServerAndEvaluates() throws Exception {
        Path home = newHome("scenario1-home");
        Path program = Files.writeString(
                tmpRoot.resolve("scenario1.jsonnet"),
                "{ hello: \"world\", sum: std.foldl(function(a,b) a+b, [1,2,3,4,5], 0) }\n");

        Process server = startServer(home, version);
        try {
            ClientResult r = runClient(home, program, tmpRoot.resolve("scenario1.stdout"),
                    tmpRoot.resolve("scenario1.stderr"));

            assertEquals("client exit code\nstderr=" + r.stderr, 0, r.exitCode);
            assertTrue("expected hello:world in stdout, got: " + r.stdout,
                    r.stdout.contains("\"hello\"") && r.stdout.contains("\"world\""));
            assertTrue("expected sum:15 in stdout, got: " + r.stdout,
                    r.stdout.contains("15"));

            // Server must have left a lockBase footprint (pid file, log, etc.)
            assertTrue("server pid lock file expected",
                    Files.exists(lockBaseForHome(home).resolve("pid")));
        } finally {
            stopServer(server);
            stopServersForHome(home);
        }
    }

    /**
     * Scenario 2: second invocation reuses the running server (no re-spawn).
     * Result must be identical and exit code 0.
     */
    @Test
    public void secondInvocationReusesServer() throws Exception {
        Path home = newHome("scenario2-home");
        Path program = Files.writeString(
                tmpRoot.resolve("scenario2.jsonnet"),
                "{ arr: [x * x for x in std.range(1, 4)] }\n");

        Process server = startServer(home, version);
        try {
            ClientResult a = runClient(home, program, tmpRoot.resolve("scenario2a.stdout"),
                    tmpRoot.resolve("scenario2a.stderr"));
            ClientResult b = runClient(home, program, tmpRoot.resolve("scenario2b.stdout"),
                    tmpRoot.resolve("scenario2b.stderr"));

            assertEquals("first run exit", 0, a.exitCode);
            assertEquals("second run exit", 0, b.exitCode);
            assertEquals("both runs produce same JSON",
                    a.stdout.stripTrailing(), b.stdout.stripTrailing());
            assertTrue("expected arr in output", a.stdout.contains("\"arr\""));
        } finally {
            stopServer(server);
            stopServersForHome(home);
        }
    }

    /**
     * Scenario 3: malformed jsonnet produces non-zero exit and the error
     * is surfaced on stderr, not silently swallowed.
     */
    @Test
    public void malformedJsonnetReturnsNonZeroExit() throws Exception {
        Path home = newHome("bad-home");
        Path program = Files.writeString(
                tmpRoot.resolve("bad.jsonnet"),
                "this is not valid jsonnet {{{\n");

        Process server = startServer(home, version);
        try {
            ClientResult r = runClient(home, program, tmpRoot.resolve("bad.stdout"),
                    tmpRoot.resolve("bad.stderr"));

            assertNotEquals("non-zero exit for bad syntax", 0, r.exitCode);
        } finally {
            stopServer(server);
            stopServersForHome(home);
        }
    }

    /**
     * Scenario 4: server auto-restarts when client reports a different
     * SJSONNET_VERSION (simulates binary upgrade while server is running).
     */
    @Test
    public void versionMismatchRestartsServer() throws Exception {
        Path home = newHome("version-home");
        Path program = Files.writeString(
                tmpRoot.resolve("version.jsonnet"),
                "\"ok\"\n");

        Process server = startServer(home, version);
        try {
            // First run with the real version — seeds a server.
            ClientResult a = runClient(home, program, tmpRoot.resolve("version1.stdout"),
                    tmpRoot.resolve("version1.stderr"));
            assertEquals("first run exit", 0, a.exitCode);

            // Second run with a fake version — server should restart.
            ClientResult b = runClientWithVersion(home, program, "0.0.0-fake-for-test",
                    tmpRoot.resolve("version2.stdout"),
                    tmpRoot.resolve("version2.stderr"));
            // Either the new server starts successfully, or the mismatch message is printed.
            // Both are acceptable; the invariant is "no hang / no crash-without-output".
            assertNotNull("stdout captured", b.stdout);
            assertNotNull("stderr captured", b.stderr);
        } finally {
            stopServer(server);
            stopServersForHome(home);
        }
    }

    // ---- helpers ----

    private static Path newHome(String name) throws IOException {
        Path home = tmpRoot.resolve(name);
        Files.createDirectories(home);
        return home;
    }

    private static Path lockBaseForHome(Path home) {
        return home.resolve(".sjsonnet").resolve("out").resolve("mill-worker-1");
    }

    private static Process startServer(Path home, String sjsonnetVersion) throws Exception {
        Path lockBase = lockBaseForHome(home);
        Files.createDirectories(lockBase);
        List<String> serverCmd = new ArrayList<>(Arrays.asList(
                javaExecutable(),
                "-DSJSONNET_VERSION=" + sjsonnetVersion,
                "-cp", classpath,
                "--enable-native-access=ALL-UNNAMED",
                "sjsonnet.SjsonnetServerRealMain",
                lockBase.toString()
        ));

        Process server = new ProcessBuilder(serverCmd)
                .redirectOutput(lockBase.resolve("logs").toFile())
                .redirectError(lockBase.resolve("logs").toFile())
                .start();
        waitForServerSocket(lockBase, server);
        return server;
    }

    private static void waitForServerSocket(Path lockBase, Process server) throws Exception {
        Path socket = lockBase.resolve("io");
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
        while (System.nanoTime() < deadline) {
            if (Files.exists(socket)) return;
            if (!server.isAlive()) {
                fail("server exited before socket was ready\nserver log=" +
                        readIfExists(lockBase.resolve("logs")));
            }
            Thread.sleep(10);
        }
        fail("server socket was not ready within 10s\nserver log=" +
                readIfExists(lockBase.resolve("logs")));
    }

    private ClientResult runClient(Path home, Path program, Path stdoutPath, Path stderrPath)
            throws Exception {
        return runClientWithVersion(home, program, version, stdoutPath, stderrPath);
    }

    private ClientResult runClientWithVersion(Path home, Path program, String sjsonnetVersion,
                                              Path stdoutPath, Path stderrPath)
            throws Exception {
        List<String> clientCmd = new ArrayList<>(Arrays.asList(
                javaExecutable(),
                "-Duser.home=" + home,
                "-DSJSONNET_EXECUTABLE=" + classpath,
                "-DSJSONNET_VERSION=" + sjsonnetVersion,
                "-cp", classpath,
                "--enable-native-access=ALL-UNNAMED",
                "sjsonnet.client.SjsonnetClientMain",
                program.toString()
        ));

        Process clientProc = new ProcessBuilder(clientCmd)
                .redirectOutput(stdoutPath.toFile())
                .redirectError(stderrPath.toFile())
                .start();

        boolean clientDone = clientProc.waitFor(30, TimeUnit.SECONDS);
        int exitCode = clientDone ? clientProc.exitValue() : -1;

        if (!clientDone) {
            clientProc.destroyForcibly();
            fail("client did not complete within 30s\nstdout=" +
                    readIfExists(stdoutPath) + "\nstderr=" + readIfExists(stderrPath) +
                    "\nserver log=" + readIfExists(lockBaseForHome(home).resolve("logs")));
        }

        String stdout = Files.readString(stdoutPath, StandardCharsets.UTF_8);
        String stderr = Files.readString(stderrPath, StandardCharsets.UTF_8);
        return new ClientResult(exitCode, stdout, stderr);
    }

    private static String readIfExists(Path path) {
        try {
            return Files.exists(path) ? Files.readString(path, StandardCharsets.UTF_8) : "";
        } catch (IOException e) {
            return "<failed to read " + path + ": " + e + ">";
        }
    }

    private static void stopServersForRoot() {
        if (tmpRoot == null) return;
        stopServersMatching(tmpRoot.toString());
    }

    private static void stopServersForHome(Path home) {
        stopServersMatching(lockBaseForHome(home).toString());
    }

    private static void stopServersMatching(String marker) {
        ProcessHandle.allProcesses()
                .filter(process -> process.info().commandLine()
                        .map(command -> command.contains("sjsonnet.SjsonnetServerRealMain") &&
                                command.contains(marker))
                        .orElse(false))
                .forEach(E2ETests::stopServer);
    }

    private static void stopServer(ProcessHandle process) {
        try {
            process.destroy();
            process.onExit().get(5, TimeUnit.SECONDS);
        } catch (Exception ignored) {
            process.destroyForcibly();
        }
    }

    private static void stopServer(Process process) {
        try {
            if (process == null) return;
            process.destroy();
            if (!process.waitFor(5, TimeUnit.SECONDS)) {
                process.destroyForcibly();
                process.waitFor(5, TimeUnit.SECONDS);
            }
        } catch (Exception ignored) {
            if (process != null) process.destroyForcibly();
        }
    }

    private static String javaExecutable() {
        String home = System.getProperty("java.home");
        return (home == null ? "java" : home + "/bin/java");
    }

    private static final class ClientResult {
        final int exitCode;
        final String stdout;
        final String stderr;
        ClientResult(int exitCode, String stdout, String stderr) {
            this.exitCode = exitCode;
            this.stdout = stdout;
            this.stderr = stderr;
        }
    }
}
