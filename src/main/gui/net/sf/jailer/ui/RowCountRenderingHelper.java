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

	private static final String THINSP = "&thinsp;";
	public String nonMGSuffix;
	public String nonMGSuffixG;
	public String nonMGSuffixM;
	public String nonMGSuffixK;
	public String nonMGPrefixG;
	public String nonMGPrefixM;
	public String nonMGPrefixK;

	public RowCountRenderingHelper() {
		nonMGSuffixG = createThinspSuffix("m", "g");
		nonMGSuffixM = createThinspSuffix("g", "m");
		nonMGSuffixK = createThinspSuffix("m", "k");
		nonMGSuffix = createThinspSuffix(nonMGSuffixK + "k", "");
		nonMGPrefixG = THINSP;
		nonMGSuffixG = nonMGSuffixG.replaceFirst(THINSP, "");
		nonMGPrefixM = THINSP;
		nonMGSuffixM = nonMGSuffixM.replaceFirst(THINSP, "");
		nonMGPrefixK = THINSP;
		nonMGSuffixK = nonMGSuffixK.replaceFirst(THINSP, "");
	}
	
	// TODO
	// TODO make component for this (panel with 2 labels?)
	// TODO re-read rowcount heuristically after DML? (ins/upd/del)?
	// TODO re-read rowcount of 0 or < 1000(?) after reading stats-info-query-result?

	private String createThinspSuffix(String target, String subject) {
		int gWidth = new JLabel("<html>0" + target + "</html>").getPreferredSize().width;
        int w = 0;
        String suffix = "";
        for (suffix = ""; suffix.length() < 160;) {
        	int pw = w;
        	String psuffix = suffix;
        	suffix += THINSP;
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

}
