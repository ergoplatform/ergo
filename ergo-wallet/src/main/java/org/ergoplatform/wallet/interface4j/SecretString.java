package org.ergoplatform.wallet.interface4j;

import java.util.Arrays;

/**
 * Direct copy from https://github.com/aslesarenko/ergo-appkit SecretString class
 * Encapsulates secret array of characters (char[]) with proper equality.
 * The secret data can be {@link SecretString#erase() erased} in memory and not leaked to GC.
 * Using this class is more secure and safe than using char[] directly.
 */
public final class SecretString {
    /**
     * Secret data, should not be copied outside of this instance.
     */
    private final char[] _data;

    /**
     * Use static methods to construct new instances.
     */
    SecretString(char[] data) {
        _data = data;
    }

    /**
     * Returns true if the string doesn't have characters.
     */
    public boolean isEmpty() { return _data == null || _data.length == 0; }

    /**
     * Extracts secret characters as an array.
     */
    public char[] getData() {
        return _data;
    }

    /**
     * Erases secret characters stored in this instance so that they are no longer reside in memory.
     */
    public void erase() {
        Arrays.fill(_data, ' ');
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(_data);
    }

    @Override
    public boolean equals(Object obj) {
        // this implementation is copied from java.lang.String
        if (this == obj) {
            return true;
        }
        if (obj instanceof SecretString) {
            SecretString anotherString = (SecretString)obj;
            int n = _data.length;
            if (n == anotherString._data.length) {
                char v1[] = _data;
                char v2[] = anotherString._data;
                int i = 0;
                while (n-- != 0) {
                    if (v1[i] != v2[i])
                        return false;
                    i++;
                }
                return true;
            }
        }
        return false;
    }

    /**
     * Creates a new instance wrapping the given characters. The given array is not copied.
     */
    public static SecretString create(char[] data) { return new SecretString(data); }

    /**
     * Creates a new instance by copying characters from the given String.
     */
    public static SecretString create(String s) { return new SecretString(s.toCharArray()); }

    /**
     * Create a new instance with empty sequence of characters.
     */
    public static SecretString empty() { return new SecretString(new char[0]); }

    /**
     * Returns unsecured String with secret characters.
     * The secret characters are copied to the new String instance and cannot be erased in memory.
     * So they leak to GC and may remain in memory until overwritten by new data.
     * Usage of this method is discouraged and the method is provided solely to interact with
     * legacy code which keeps secret characters in String.
     */
    public String toStringUnsecure() {
        return String.valueOf(_data);
    }
}