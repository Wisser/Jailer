package net.sf.jailer.util;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipInputStream;

public class LGTMPotentialInputResourceLeakTest {
	
	public void test() throws IOException {
		InputStream in = new ZipInputStream(new FileInputStream("test.zip"));
		in.close();
	}
}
