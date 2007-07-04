package org.jailer.ui;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import javax.swing.JTextField;

/**
 * Manages settings of formulars.
 * 
 * @author Wisser
 */
public class Settings  {

    /**
     * The formular fields.
     */
    private final Map<String, JTextField> fields;

    /**
     * The name of the file holding the settings.
     */
    private final String FILENAME;
    
    /**
     * Holds field values for each setting.
     */
    private Map<String, Map<String, String>> settings = new TreeMap<String, Map<String,String>>();
    
    public String currentSetting = null;
    
    /**
     * Constructor.
     * 
     * @param fields the formular fields
     */
    public Settings(String fileName, Map<String, JTextField> fields) {
        this.FILENAME = fileName;
        this.fields = fields;
        if (new File(FILENAME).exists()) {
            try {
                ObjectInputStream in = new ObjectInputStream(new FileInputStream(FILENAME));
                settings = (Map<String, Map<String, String>>) in.readObject();
                currentSetting = (String) in.readObject();
                in.close();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Gets names of all settings.
     * 
     * @return names of all settings
     */
    public Object[] getSettingNames() {
        return settings.keySet().toArray();
    }

    /**
     * Saves a setting.
     * 
     * @param name the name of the setting
     */
    public void save(String name) {
        if (name != null && name.trim().length() > 0) {
            Map<String, String> setting = new HashMap<String, String>();
            for (Map.Entry<String, JTextField> entry: fields.entrySet()) {
                setting.put(entry.getKey(), entry.getValue().getText());
            }
            settings.put(name.trim(), setting);
            currentSetting = name;
            try {
                ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(FILENAME));
                out.writeObject(settings);
                out.writeObject(currentSetting);
                out.close();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Restores a setting.
     * 
     * @param name the name of the setting
     */
    public void restore(String name) {
        if (name != null && name.trim().length() > 0) {
            Map<String, String> setting = settings.get(name.trim());
            if (setting != null) {
                for (Map.Entry<String, JTextField> entry: fields.entrySet()) {
                    entry.getValue().setText(setting.get(entry.getKey()));
                }
            }
        }
    }
    
}
