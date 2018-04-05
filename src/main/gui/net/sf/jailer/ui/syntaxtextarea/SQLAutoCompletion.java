/*
 * Copyright 2007 - 2018 the original author or authors.
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

import java.awt.Color;
import java.awt.Component;

import javax.swing.JLabel;
import javax.swing.JList;

import org.fife.ui.autocomplete.AutoCompletion;
import org.fife.ui.autocomplete.BasicCompletion;
import org.fife.ui.autocomplete.Completion;
import org.fife.ui.autocomplete.CompletionCellRenderer;
import org.fife.ui.autocomplete.CompletionProvider;
import org.fife.ui.autocomplete.Util;
import org.fife.ui.rtextarea.RTextArea;


/**
 * Auto completions for SQL statements.
 * 
 * @author Ralf Wisser
 */
public class SQLAutoCompletion extends AutoCompletion {
	
	private final RTextArea editorPane;

	public SQLAutoCompletion(CompletionProvider provider, RTextArea editorPane) {
		super(provider);
		this.editorPane = editorPane;
		install(editorPane);
		setListCellRenderer(new CompletionCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList list, Object value, int index, boolean selected,
					boolean hasFocus) {
				Component c = super.getListCellRendererComponent(list, value, index, selected, hasFocus);
				if (c instanceof JLabel && value instanceof SQLCompletionProvider.SQLCompletion) {
					((JLabel) c).setToolTipText(((SQLCompletionProvider.SQLCompletion) value).tooltip);
				}
				return c;
			}

			@Override
			protected void prepareForOtherCompletion(JList list,
				Completion c, int index, boolean selected, boolean hasFocus) {

				Color color = null;
				if (c instanceof SQLCompletionProvider.SQLCompletion) {
					color = ((SQLCompletionProvider.SQLCompletion) c).color;
				}
				
				StringBuilder sb = new StringBuilder("<html><nobr>");
				if (!selected && color != null) {
					sb.append("<font color='").append(Util.getHexString(color)).append("'>");
				}
				sb.append(c.getInputText());
				if (!selected && color != null) {
					sb.append("</font>");
				}

				if (c instanceof BasicCompletion) {
					String definition = ((BasicCompletion)c).getShortDescription();
					if (definition!=null) {
						sb.append(" - ");
						if (!selected) {
							sb.append("<font color='").append(Util.getHexString(Color.gray)).append("'>");
						}
						sb.append(definition);
						if (!selected) {
							sb.append("</font>");
						}
					}
				}
				
				setText(sb.toString());
			}
		});
	}

	@Override
	protected void insertCompletion(Completion c, boolean typedParamListStartChar) {
		editorPane.beginAtomicEdit();
		super.insertCompletion(c, typedParamListStartChar);
		editorPane.endAtomicEdit();
	}

};
