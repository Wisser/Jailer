# Jailer HOWTO

Please open this in your IDE as a Maven project.

## Building

```shell
 ./mvnw clean package
```

After building it, you can run the jailer.sh script to execute the command line.

To build the stand-alone jar:
```shell
 ./mvnw clean package --enable-profiles executable-jar
```

Stand alone jar is easier to use in batch projects, because only a single file needs to be copied.

## Running the Jailer GUI

After building the project you can execute [jailerGUI.sh](jailerGUI.sh).

Or, you can run (or debug) GUI in IDE: to do that simply open [`JailerUI.java`](jailer-gui/src/main/gui/net/sf/jailer/ui/JailerUI.java) in your IDE and launch it.
