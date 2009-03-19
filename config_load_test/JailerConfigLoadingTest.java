import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;


public class JailerConfigLoadingTest {

	public static void main(String[] args) {
		System.out.println(System.getProperties());
		
		String cf = "config" + File.separator + "config.xml";
		System.out.println(cf);
		File config = new File(cf);
		System.out.println(config.exists());
		try {
			FileInputStream in = new FileInputStream(config);
			while (in.read() != -1) {}
			System.out.println(cf + " read");
		} catch (Exception e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}

}
