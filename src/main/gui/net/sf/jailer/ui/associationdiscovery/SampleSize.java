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
package net.sf.jailer.ui.associationdiscovery;

import javax.swing.JComboBox;

import net.sf.jailer.ui.util.UISettings;

/**
 * The maximum number of rows of a table that are read when only a sample of it is checked.
 * Shared by the association discovery and the normalization check, and stored in the {@link UISettings}.
 *
 * @author Ralf Wisser
 */
public class SampleSize {

	/**
	 * Means: no limit, the whole table is read.
	 */
	public static final int UNLIMITED = 0;

	private static final String SETTINGS_KEY = "discoverySampleRows";

	private static final int DEFAULT = 100000;

	private static final int[] SIZES = { 10000, 100000, 1000000, UNLIMITED };

	private static final String[] TEXTS = { "10,000", "100,000", "1,000,000", "unlimited" };

	/**
	 * Gets the stored sample size.
	 *
	 * @return the number of rows, or {@link #UNLIMITED}
	 */
	public static int get() {
		Object value = UISettings.restore(SETTINGS_KEY);
		if (value instanceof Integer) {
			for (int size: SIZES) {
				if (size == (Integer) value) {
					return size;
				}
			}
		}
		return DEFAULT;
	}

	/**
	 * Creates a combo box for choosing the sample size, preset with the stored one.
	 * A choice is stored at once.
	 *
	 * @param onChange called after the user has chosen another size, or <code>null</code>
	 * @return the combo box
	 */
	public static JComboBox<String> createComboBox(Runnable onChange) {
		JComboBox<String> comboBox = new JComboBox<String>(TEXTS);
		int current = get();
		for (int i = 0; i < SIZES.length; ++i) {
			if (SIZES[i] == current) {
				comboBox.setSelectedIndex(i);
			}
		}
		comboBox.addActionListener(e -> {
			int index = comboBox.getSelectedIndex();
			if (index >= 0 && SIZES[index] != get()) {
				UISettings.store(SETTINGS_KEY, SIZES[index]);
				if (onChange != null) {
					onChange.run();
				}
			}
		});
		return comboBox;
	}

}
