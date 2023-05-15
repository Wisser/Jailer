package net.sf.jailer.ui.util;

import java.awt.Cursor;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

/*
 *  Prevent the specified number of columns from scrolling horizontally in
 *  the scroll pane. The table must already exist in the scroll pane.
 *
 *  The functionality is accomplished by creating a second JTable (fixed)
 *  that will share the TableModel and SelectionModel of the main table.
 *  This table will be used as the row header of the scroll pane.
 *
 *  The fixed table created can be accessed by using the getFixedTable()
 *  method. will be returned from this method. It will allow you to:
 *
 *  You can change the model of the main table and the change will be
 *  reflected in the fixed model. However, you cannot change the structure
 *  of the model.
 */
public class FixedColumnTable implements ChangeListener, PropertyChangeListener
{
	private JTable main;
	public JTable fixed;
	private JScrollPane scrollPane;

	/*
	 *  Specify the number of columns to be fixed and the scroll pane
	 *  containing the table.
	 */
	public FixedColumnTable(int fixedColumns, JScrollPane scrollPane)
	{
		this.scrollPane = scrollPane;

		main = ((JTable)scrollPane.getViewport().getView());
		main.setAutoCreateColumnsFromModel( false );
		main.addPropertyChangeListener( this );

		//  Use the existing table to create a new table sharing
		//  the DataModel and ListSelectionModel

		fixed = new JTable();
		fixed.setAutoCreateColumnsFromModel( false );
		fixed.setModel(main.getModel());
		fixed.setDefaultRenderer(Object.class, main.getDefaultRenderer(Object.class));
		fixed.setName("columnNames");

		//  Remove the fixed columns from the main table
		//  and add them to the fixed table

		for (int i = 0; i < fixedColumns; i++)
		{
	        TableColumnModel columnModel = main.getColumnModel();
	        TableColumn column = columnModel.getColumn(0);
	        TableColumn column2 = new TableColumn(0);
	        
	        column2.setHeaderValue(column.getHeaderValue());
	        column2.setMinWidth(column.getMinWidth());
	        column2.setMaxWidth(column.getMaxWidth());
	        column2.setWidth(column.getWidth());
	        column2.setPreferredWidth(column.getPreferredWidth());
	        
	        column.setWidth(0);
	        column.setMinWidth(0);
	        column.setMaxWidth(0); 

	        // columnModel.removeColumn(column);
			fixed.getColumnModel().addColumn(column2);
		}

		//  Add the fixed table to the scroll pane

        fixed.setPreferredScrollableViewportSize(fixed.getPreferredSize());
		scrollPane.setRowHeaderView( fixed );
		scrollPane.setCorner(JScrollPane.UPPER_LEFT_CORNER, fixed.getTableHeader());

		// Synchronize scrolling of the row header with the main table

		scrollPane.getRowHeader().addChangeListener( this );
		
		MouseAdapter ma = new MouseAdapter()
		{
		    TableColumn column;
		    int columnWidth;
		    int pressedX;

		    public void mousePressed(MouseEvent e)
		    {
		        JTableHeader header = (JTableHeader)e.getComponent();
		        TableColumnModel tcm = header.getColumnModel();
		        int columnIndex = tcm.getColumnIndexAtX( e.getX() );
		        Cursor cursor = header.getCursor();

		        if (columnIndex == tcm.getColumnCount() - 1
		        &&  cursor == Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR))
		        {
		            column = tcm.getColumn( columnIndex );
		            columnWidth = column.getWidth();
		            pressedX = e.getX();
		            header.addMouseMotionListener( this );
		        }
		    }

		    public void mouseReleased(MouseEvent e)
		    {
		        JTableHeader header = (JTableHeader)e.getComponent();
		        header.removeMouseMotionListener( this );
		    }

		    public void mouseDragged(MouseEvent e)
		    {
		        int width = columnWidth - pressedX + e.getX();
		        column.setPreferredWidth( width );
		        JTableHeader header = (JTableHeader)e.getComponent();
		        JTable table = header.getTable();
		        table.setPreferredScrollableViewportSize(table.getPreferredSize());
		        JScrollPane scrollPane = (JScrollPane)table.getParent().getParent();
		        scrollPane.revalidate();
		    }
		};

		JTable fixed = getFixedTable();
		fixed.getTableHeader().addMouseListener( ma );
	}

	/*
	 *  Return the table being used in the row header
	 */
	public JTable getFixedTable()
	{
		return fixed;
	}
//
//  Implement the ChangeListener
//
	public void stateChanged(ChangeEvent e)
	{
		//  Sync the scroll pane scrollbar with the row header

		JViewport viewport = (JViewport) e.getSource();
		scrollPane.getVerticalScrollBar().setValue(viewport.getViewPosition().y);
	}
//
//  Implement the PropertyChangeListener
//
	public void propertyChange(PropertyChangeEvent e)
	{
		//  Keep the fixed table in sync with the main table

		if ("selectionModel".equals(e.getPropertyName()))
		{
			fixed.setSelectionModel( main.getSelectionModel() );
		}

		if ("model".equals(e.getPropertyName()))
		{
			fixed.setModel( main.getModel() );
		}
	}
}
