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
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * Removes UTF-8 BOMs from text files.
 */
public class BomRemover {

    private static final byte[] UTF8_BOM = { (byte) 0xEF, (byte) 0xBB, (byte) 0xBF };

    private static final Set<String> EXTENSIONS = new HashSet<>(Arrays.asList(
            "java", "html", "htm", "xml", "json", "txt", "csv", "properties", "sql", "md"
    ));

    /**
     * Recursively removes UTF-8 BOMs from all text files under the given directory.
     *
     * @return number of files fixed
     */
    public static int removeBOMs(File directory) throws IOException {
        int count = 0;
        File[] files = directory.listFiles();
        if (files == null) {
            return 0;
        }
        for (File file : files) {
            if (file.isDirectory()) {
                if (!file.getName().equals(".git")) {
                    count += removeBOMs(file);
                }
            } else if (hasTextExtension(file)) {
                if (removeBOM(file)) {
                    System.out.println("Removed BOM: " + file.getPath());
                    count++;
                }
            }
        }
        return count;
    }

    private static boolean removeBOM(File file) throws IOException {
        byte[] bytes = Files.readAllBytes(file.toPath());
        if (bytes.length >= 3
                && bytes[0] == UTF8_BOM[0]
                && bytes[1] == UTF8_BOM[1]
                && bytes[2] == UTF8_BOM[2]) {
            byte[] stripped = Arrays.copyOfRange(bytes, 3, bytes.length);
            Files.write(file.toPath(), stripped);
            return true;
        }
        return false;
    }

    private static boolean hasTextExtension(File file) {
        String name = file.getName();
        int dot = name.lastIndexOf('.');
        return dot >= 0 && EXTENSIONS.contains(name.substring(dot + 1).toLowerCase());
    }

    public static void main(String[] args) throws IOException {
        File root = args.length > 0 ? new File(args[0]) : new File(".");
        int count = removeBOMs(root);
        System.out.println(count + " file(s) fixed.");
    }
}
