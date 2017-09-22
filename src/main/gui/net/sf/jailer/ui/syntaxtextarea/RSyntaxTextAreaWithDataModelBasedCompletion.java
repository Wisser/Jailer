/*
 * Copyright 2007 - 2017 the original author or authors.
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
package net.sf.jailer.ui.syntaxtextarea;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.KeyStroke;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;

import net.sf.jailer.datamodel.DataModel;

/**
 * Text area with code completion based on {@link DataModel} information.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class RSyntaxTextAreaWithDataModelBasedCompletion extends RSyntaxTextArea {
	
	public RSyntaxTextAreaWithDataModelBasedCompletion() {
		setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SQL);
		
		Action action = new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				RSyntaxTextAreaWithDataModelBasedCompletion.this.actionPerformed();
			}
		};
		
		KeyStroke ks = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK);
		InputMap im = getInputMap();
		im.put(ks, action);
		ActionMap am = getActionMap();
		am.put(action, action);
	}

	protected void actionPerformed() {
	}
	
}
