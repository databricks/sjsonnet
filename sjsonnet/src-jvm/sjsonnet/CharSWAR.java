package sjsonnet;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;

/**
 * SWAR (SIMD Within A Register) utilities for JSON string rendering and string comparison.
 *
 * <p>Provides:
 * <ul>
 *   <li>Escape-char scanning: detects/locates chars requiring JSON escaping
 *       (control chars, double-quote, backslash).</li>
 *   <li>String comparison: codepoint-correct comparison with array-based inner loop
 *       that the JIT can auto-vectorize to SIMD instructions.</li>
 * </ul>
 *
 * <p>For strings above a threshold length, converts to ISO-8859-1 bytes and
 * processes 8 bytes at a time using {@link VarHandle} bulk reads + Hacker's
 * Delight zero-detection formula. For shorter strings, uses a scalar charAt loop.
 *
 * <p>Based on the SWAR technique from Hacker's Delight Ch. 6, as used by
 * <a href="https://github.com/netty/netty/blob/4.2/common/src/main/java/io/netty/util/internal/SWARUtil.java">
 * Netty SWARUtil</a> and
 * <a href="https://github.com/apache/pekko/blob/main/actor/src/main/scala/org/apache/pekko/util/SWARUtil.scala">
 * Apache Pekko SWARUtil</a>.
 *
 * @see <a href="https://richardstartin.github.io/posts/finding-bytes">Finding Bytes in Arrays</a>
 */
public final class CharSWAR {
    private CharSWAR() {}

    // VarHandle for reading longs from byte[] — replaces sun.misc.Unsafe.
    // Following Netty VarHandleFactory pattern:
    //   MethodHandles.byteArrayViewVarHandle(long[].class, ByteOrder)
    private static final VarHandle LONG_VIEW =
            MethodHandles.byteArrayViewVarHandle(long[].class, ByteOrder.nativeOrder());

    // --- 8-bit SWAR constants (Netty/Pekko pattern) ---
    //
    // Hacker's Delight zero-detection for 8-bit lanes:
    //   input = word ^ pattern          // zero bytes where byte matches
    //   tmp = (input & 0x7F7F...) + 0x7F7F...  // carry into bit 7 iff non-zero
    //   result = ~(tmp | input | 0x7F7F...)     // bit 7 set iff lane was zero

    private static final long HOLE  = 0x7F7F_7F7F_7F7F_7F7FL;

    /** Broadcast '"' (0x22) to all 8 byte lanes. */
    private static final long QUOTE = 0x2222_2222_2222_2222L;

    /** Broadcast '\\' (0x5C) to all 8 byte lanes. */
    private static final long BSLAS = 0x5C5C_5C5C_5C5C_5C5CL;

    /** Mask for bits 5-7 of each byte; zero result means byte < 32. */
    private static final long CTRL  = 0xE0E0_E0E0_E0E0_E0E0L;

    /** Below this length, scalar charAt is faster than SWAR + byte[] conversion. */
    private static final int SWAR_THRESHOLD = 128;

    /**
     * Check if any char in {@code str} needs JSON string escaping.
     * Scan-first API: call on the String before copying to the output buffer.
     */
    public static boolean hasEscapeChar(String str) {
        int len = str.length();
        if (len < SWAR_THRESHOLD) {
            return hasEscapeCharScalar(str, len);
        }
        // ISO-8859-1 encoding is a JVM intrinsic for LATIN1 compact strings —
        // essentially a memcpy of the internal byte[]. Chars > 255 map to '?'
        // (0x3F), which is safe (not a control char, not '"', not '\\').
        byte[] bytes = str.getBytes(StandardCharsets.ISO_8859_1);
        return hasEscapeCharSWAR(bytes, 0, bytes.length);
    }

    /**
     * Check if any byte in {@code arr[from..to)} needs JSON string escaping.
     * Used by ByteRenderer for in-place SWAR scan on byte[] buffers.
     * UTF-8 multi-byte sequences never produce bytes matching '"', '\\', or &lt; 0x20,
     * so this is safe for scanning UTF-8 encoded data.
     */
    public static boolean hasEscapeChar(byte[] arr, int from, int to) {
        return hasEscapeCharSWAR(arr, from, to);
    }

    /**
     * Check if any char in {@code arr[from..to)} needs JSON string escaping.
     */
    public static boolean hasEscapeChar(char[] arr, int from, int to) {
        for (int i = from; i < to; i++) {
            char c = arr[i];
            if (c < 32 || c == '"' || c == '\\') return true;
        }
        return false;
    }

    private static boolean hasEscapeCharSWAR(byte[] arr, int from, int to) {
        int i = from;
        int limit = to - 7; // 8 bytes per VarHandle.get
        while (i < limit) {
            long word = (long) LONG_VIEW.get(arr, i);
            if (swarHasMatch(word)) return true;
            i += 8;
        }
        // Tail: remaining 0-7 bytes
        while (i < to) {
            int b = arr[i] & 0xFF;
            if (b < 32 || b == '"' || b == '\\') return true;
            i++;
        }
        return false;
    }

    /**
     * 8-bit SWAR: returns true if any byte lane in {@code word}
     * contains '"' (0x22), '\\' (0x5C), or a control char (&lt; 0x20).
     *
     * <p>Uses Netty/Pekko pattern: XOR to produce zero lanes, then
     * Hacker's Delight formula to detect zero bytes.
     */
    private static boolean swarHasMatch(long word) {
        // 1. Detect '"' via XOR + zero-detection (Netty SWARUtil.applyPattern)
        long q = word ^ QUOTE;
        long qz = ~((q & HOLE) + HOLE | q | HOLE);

        // 2. Detect '\\' via XOR + zero-detection
        long b = word ^ BSLAS;
        long bz = ~((b & HOLE) + HOLE | b | HOLE);

        // 3. Detect control chars: byte & 0xE0 == 0 means bits 5-7 all zero → c < 32
        long c = word & CTRL;
        long cz = ~((c & HOLE) + HOLE | c | HOLE);

        return (qz | bz | cz) != 0L;
    }

    /** Scalar scan for String (used for short strings). */
    private static boolean hasEscapeCharScalar(String s, int len) {
        for (int i = 0; i < len; i++) {
            char c = s.charAt(i);
            if (c < 32 || c == '"' || c == '\\') return true;
        }
        return false;
    }

    // =========================================================================
    // findFirstEscapeChar — position-returning SWAR scan for chunked rendering
    // =========================================================================

    /**
     * Find the index of the first byte in {@code arr[from..to)} that needs JSON
     * string escaping. Returns {@code -1} if no escape char is found.
     *
     * <p>Uses SWAR to scan 8 bytes per iteration, then pinpoints the exact byte
     * within a matched 8-byte word via scalar fallback.
     */
    public static int findFirstEscapeChar(byte[] arr, int from, int to) {
        int i = from;
        int limit = to - 7;
        while (i < limit) {
            long word = (long) LONG_VIEW.get(arr, i);
            if (swarHasMatch(word)) {
                // Pinpoint exact byte within the matched 8-byte word
                for (int j = i; j < i + 8; j++) {
                    int b = arr[j] & 0xFF;
                    if (b < 32 || b == '"' || b == '\\') return j;
                }
            }
            i += 8;
        }
        // Tail: remaining 0-7 bytes
        while (i < to) {
            int b = arr[i] & 0xFF;
            if (b < 32 || b == '"' || b == '\\') return i;
            i++;
        }
        return -1;
    }

    /**
     * Find the index of the first char in {@code arr[from..to)} that needs JSON
     * string escaping. Returns {@code -1} if no escape char is found.
     * Scalar scan on char[] — used by char-based chunked rendering.
     */
    public static int findFirstEscapeCharChar(char[] arr, int from, int to) {
        for (int i = from; i < to; i++) {
            char c = arr[i];
            if (c < 32 || c == '"' || c == '\\') return i;
        }
        return -1;
    }

    // =========================================================================
    // isAllAscii — check if all chars are ASCII (< 0x80)
    // =========================================================================

    /**
     * Returns true if all characters in the string are ASCII (&lt; 0x80).
     * Uses ISO-8859-1 encoding + SWAR for long strings. For ASCII-only strings,
     * codepoint operations can be replaced with direct char indexing.
     */
    public static boolean isAllAscii(String s) {
        int len = s.length();
        for (int i = 0; i < len; i++) {
            if (s.charAt(i) >= 0x80) return false;
        }
        return true;
    }

    // =========================================================================
    // compareStrings — JIT-vectorizable codepoint-correct string comparison
    // =========================================================================

    /** Reusable char buffers for string comparison (one per thread). */
    private static final int CMP_BUF_SIZE = 32768;
    private static final ThreadLocal<char[]> CMP_BUF1 =
            ThreadLocal.withInitial(() -> new char[CMP_BUF_SIZE]);
    private static final ThreadLocal<char[]> CMP_BUF2 =
            ThreadLocal.withInitial(() -> new char[CMP_BUF_SIZE]);

    /** Below this length, scalar charAt comparison is faster than getChars + array loop. */
    private static final int CMP_THRESHOLD = 16;

    /**
     * Compare two strings by Unicode codepoint values. Equivalent to
     * {@code Util.compareStringsByCodepoint} but uses bulk {@code getChars} +
     * tight array loop so the JIT can auto-vectorize the comparison to SIMD
     * instructions (AVX2/SSE on x86, NEON on ARM).
     *
     * <p>Surrogate checks are deferred to the mismatch point (O(1) instead of
     * O(n)), which is correct because equal chars — even surrogates — can be
     * skipped without affecting ordering.
     */
    public static int compareStrings(String s1, String s2) {
        if (s1 == s2) return 0;
        int n1 = s1.length(), n2 = s2.length();
        int minLen = Math.min(n1, n2);

        // Short strings or strings exceeding buffer: scalar path
        if (minLen < CMP_THRESHOLD || n1 > CMP_BUF_SIZE || n2 > CMP_BUF_SIZE) {
            return compareStringsScalar(s1, n1, s2, n2);
        }

        // Bulk-copy to char arrays — eliminates String.charAt() virtual dispatch,
        // enabling the JIT to auto-vectorize the comparison loop.
        char[] c1 = CMP_BUF1.get();
        char[] c2 = CMP_BUF2.get();
        s1.getChars(0, n1, c1, 0);
        s2.getChars(0, n2, c2, 0);

        // Tight comparison loop — the simple c1[i] != c2[i] pattern is what
        // the C2 JIT compiler recognizes and vectorizes.
        int i = 0;
        while (i < minLen) {
            if (c1[i] != c2[i]) {
                char a = c1[i], b = c2[i];
                if (!Character.isSurrogate(a) && !Character.isSurrogate(b)) {
                    return a - b;
                }
                // Back up if we landed on a low surrogate that's part of a pair
                int pos = i;
                if (pos > 0 && Character.isLowSurrogate(a) && Character.isHighSurrogate(c1[pos - 1])) {
                    pos--;
                }
                return compareCodepointsFrom(c1, n1, c2, n2, pos);
            }
            i++;
        }
        return Integer.compare(n1, n2);
    }

    /**
     * Scalar codepoint comparison for short strings or overflow.
     * Uses the equal-char-skip fast path (no surrogate check on matching chars).
     */
    private static int compareStringsScalar(String s1, int n1, String s2, int n2) {
        int minLen = Math.min(n1, n2);
        int i = 0;
        while (i < minLen) {
            char c1 = s1.charAt(i);
            char c2 = s2.charAt(i);
            if (c1 == c2) {
                i++;
            } else if (!Character.isSurrogate(c1) && !Character.isSurrogate(c2)) {
                return c1 - c2;
            } else {
                int cp1 = Character.codePointAt(s1, i);
                int cp2 = Character.codePointAt(s2, i);
                if (cp1 != cp2) return Integer.compare(cp1, cp2);
                i += Character.charCount(cp1);
            }
        }
        return Integer.compare(n1, n2);
    }

    /**
     * Codepoint-level comparison from a given position in char arrays.
     * Used as fallback when a mismatch involves surrogate chars.
     */
    private static int compareCodepointsFrom(char[] c1, int n1, char[] c2, int n2, int from) {
        int i1 = from, i2 = from;
        while (i1 < n1 && i2 < n2) {
            int cp1 = Character.codePointAt(c1, i1);
            int cp2 = Character.codePointAt(c2, i2);
            if (cp1 != cp2) return Integer.compare(cp1, cp2);
            i1 += Character.charCount(cp1);
            i2 += Character.charCount(cp2);
        }
        if (i1 < n1) return 1;
        if (i2 < n2) return -1;
        return 0;
    }
}
