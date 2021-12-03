package net.sf.jailer.ui.util;

import java.io.UnsupportedEncodingException;

import net.sf.jailer.util.Base64;

/**
 * Obfuscates Strings.
 * 
 * @see https://stackoverflow.com/questions/1205135/how-to-encrypt-string-in-java
 */
public class StringObfuscator {
	private static final String KEY = "Open Secret.";

	public String encrypt(final String text) {
		try {
			return Base64.encodeBytes(this.xor(text.getBytes("UTF-8")));
		} catch (java.io.UnsupportedEncodingException ex) {
			throw new IllegalStateException(ex);
		}
	}

	public String decrypt(final String text) {
		try {
			return new String(this.xor(Base64.decode(text)), "UTF-8");
		} catch (java.io.UnsupportedEncodingException ex) {
			throw new IllegalStateException(ex);
		}
	}

	private byte[] xor(final byte[] input) throws UnsupportedEncodingException {
		final byte[] output = new byte[input.length];
		final byte[] secret = KEY.getBytes("UTF-8");
		int spos = 0;
		for (int pos = 0; pos < input.length; ++pos) {
			output[pos] = (byte) (input[pos] ^ secret[spos]);
			spos += 1;
			if (spos >= secret.length) {
				spos = 0;
			}
		}
		return output;
	}
}