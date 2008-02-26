package net.sf.jailer.aliases;

/**
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 09.02.2008
 * @time: 22:19:38
 */
public class DriverNotFoundException extends Exception {

	protected String myRequestedServer;
	
	public DriverNotFoundException(String requestedServer) {
		super("Driver for " + requestedServer + " has not found");
		myRequestedServer = requestedServer;
	}

	public DriverNotFoundException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Returns a server type for which driver has been requested.
	 *
	 * @return a server type for which driver has been requested.
	 */
	public String getRequestedServer() {
		return myRequestedServer;
	}

}
