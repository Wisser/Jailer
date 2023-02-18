/*
 * Copyright 2007 - 2023 Ralf Wisser.
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
package net.sf.jailer.ui;

import javax.swing.JLabel;

/**
 * Some utility methods.
 *
 * @author Ralf Wisser
 */
public class RowCountRenderingHelper {

	public String nonMGSuffix;
	public String nonMGSuffixG;
	public String nonMGSuffixM;

	public RowCountRenderingHelper() {
		nonMGSuffixG = createThinspSuffix("m", "G");
		nonMGSuffixM = createThinspSuffix("g", "M");
		String suffix1 = createThinspSuffix(nonMGSuffixM + "m", "");
		String suffix2 = createThinspSuffix(nonMGSuffixG + "g", "");
		nonMGSuffix = suffix1.length() > suffix2.length() ? suffix1 : suffix2;
	}

	private String createThinspSuffix(String target, String subject) {
		int gWidth = new JLabel("<html>0" + target + "</html>").getPreferredSize().width;
        int w = 0;
        String suffix = "";
        for (suffix = ""; suffix.length() < 160;) {
        	int pw = w;
        	String psuffix = suffix;
        	suffix += "&thinsp;";
        	w = new JLabel("<html>0" + suffix + subject + "</html>").getPreferredSize().width;
        	if (w != 0 && w > gWidth) {
        		if (w - gWidth > gWidth - pw) {
        			suffix = psuffix;
        		}
        		break;
        	}
        }
		return suffix;
	}

	// TODO
	// TODO test with jdk8
	// TODO test with linux, both LAF

}
