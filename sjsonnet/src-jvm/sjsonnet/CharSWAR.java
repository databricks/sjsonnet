package sjsonnet;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;

/**
 * SWAR (SIMD Within A Register) escape-char scanner for JSON string rendering.
 *
 * <p>Detects characters requiring JSON escaping: control chars ({@code < 32}),
 * double-quote ({@code '"'}), and backslash ({@code '\\'}).
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
final class CharSWAR {
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
    static boolean hasEscapeChar(String str) {
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
     * Check if any char in {@code arr[from..to)} needs JSON string escaping.
     */
    static boolean hasEscapeChar(char[] arr, int from, int to) {
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
}
