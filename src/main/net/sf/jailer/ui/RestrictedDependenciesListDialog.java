/*
 * Copyright 2007 - 2012 the original author or authors.
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.AssociationListUI.AssociationModel;
import net.sf.jailer.ui.AssociationListUI.DefaultAssociationModel;

/**
 * Restricted Dependencies List Dialog.
 * 
 * @author Ralf Wisser
 */
public abstract class RestrictedDependenciesListDialog extends ClosureBorderDialog {

    private static final long serialVersionUID = -7151994890007647782L;
    
    /** Creates new form ClosureBorderDialog */
    public RestrictedDependenciesListDialog(java.awt.Frame parent) {
        super(parent, false);
        setTitle("Restricted Dependencies");
        setLocation(60, 120);
        setSize(500, 500);
        setAlwaysOnTop(true);
        refresh();
    }

    /**
     * Refreshes view after model changes.
     */
	public void refresh() {
		DataModel datamodel = getDataModel();
 		if (datamodel != null) {
			Set<Association> restDeps = new HashSet<Association>();
			for (Table table: datamodel.getTables()) {
				for (Association association: table.associations) {
					if (association.getRestrictionCondition() != null) {
						if (association.isInsertDestinationBeforeSource()) {
							restDeps.add(association);
						}
					}
				}
			}
			Collection<AssociationModel> model = new ArrayList<AssociationListUI.AssociationModel>();
			for (Association association: restDeps) {
				model.add(new DefaultAssociationModel(association));
			}
			associationListUI.setModel(model);
		}
	}

}
