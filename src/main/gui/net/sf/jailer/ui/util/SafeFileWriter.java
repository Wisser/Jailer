/*
 * Copyright 2007 - 2026 Ralf Wisser.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.sf.jailer.ui.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;

/**
 * Writes serialized objects into a file such that the file either keeps its old content
 * or gets the complete new content, but is never left truncated.
 */
public class SafeFileWriter {

	/**
	 * Writes the objects into a temporary file in the same directory and then
	 * moves it over the target file.
	 *
	 * @param file the target file
	 * @param objects the objects to write, in this order
	 */
	public static void writeObjects(File file, Object... objects) throws IOException {
		File target = file.getAbsoluteFile();
		File tmp = File.createTempFile("tmp_" + target.getName(), ".tmp", target.getParentFile());
		try {
			try (ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(tmp))) {
				for (Object object: objects) {
					out.writeObject(object);
				}
			}
			try {
				Files.move(tmp.toPath(), target.toPath(), StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
			} catch (AtomicMoveNotSupportedException e) {
				Files.move(tmp.toPath(), target.toPath(), StandardCopyOption.REPLACE_EXISTING);
			}
		} finally {
			tmp.delete();
		}
	}

}
