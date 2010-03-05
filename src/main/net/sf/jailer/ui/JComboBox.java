/*
 * Copyright 2007 the original author or authors.
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

import java.awt.Dimension;

/**
 * Workaround for Swing bug 4618607.
 * See http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4618607
 * 
 * @author Ralf Wisser
 */
public class JComboBox extends javax.swing.JComboBox {
	private static final long serialVersionUID = 1404824459186814788L;
	private boolean layingOut = false;

    public void doLayout() {
        try {
            layingOut = true;
            super.doLayout();
        }
        finally {
            layingOut = false;
        }
    }

    public Dimension getSize() {
        Dimension sz = super.getSize();
        if (!layingOut) {
            sz.width = Math.max(sz.width, getPreferredSize().width);
        }
        return sz;
    }
}
