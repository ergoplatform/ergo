package scorex;

import java.util.Comparator;

public class ByteUtils {
    /**
     * Compares primitive Byte Arrays.
     * It uses unsigned binary comparation; so the byte with negative value is always higher than byte with non-negative value.
     */
    public static final Comparator<byte[]> BYTE_ARRAY_COMPARATOR = (o1, o2) -> compare(o1, o2);

    public static int compare(byte[] o1, byte[] o2) {
//            if (o1 == o2) return 0;
        final int len = Math.min(o1.length, o2.length);
        for (int i = 0; i < len; i++) {
            int b1 = o1[i] & 0xFF;
            int b2 = o2[i] & 0xFF;
            if (b1 != b2)
                return b1 - b2;
        }
        return o1.length - o2.length;
    }


    public static int byteArrayHashCode(byte[] data) {
        //do not use Arrays.hashCode, it generates too many collisions (31 is too low)
        int h = 1;
        for (byte b : data) {
            h = h * (-1640531527) + b;
        }
        return h;
    }

    /**  returns byte array, which is greater than any other array of given size */
    public static byte[] greatest(int size){
        byte[] ret = new byte[size];
        for(int i=0;i<size;i++){
            ret[i] = (byte) 0xFF;
        }
        return ret;
    }

    public static long getLong(byte[] buf, int pos) {
        return
                ((((long) buf[pos++]) << 56) |
                        (((long) buf[pos++] & 0xFF) << 48) |
                        (((long) buf[pos++] & 0xFF) << 40) |
                        (((long) buf[pos++] & 0xFF) << 32) |
                        (((long) buf[pos++] & 0xFF) << 24) |
                        (((long) buf[pos++] & 0xFF) << 16) |
                        (((long) buf[pos++] & 0xFF) << 8) |
                        (((long) buf[pos] & 0xFF)));

    }
}
