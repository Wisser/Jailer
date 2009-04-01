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
package net.sf.jailer;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Compares XML-documents.
 *  
 * @author Ralf Wisser
 */
public class XmlDocumentComparator {

    /**
     * Checks whether all information in a xml-document
     * can also be found in another document.
     * 
     * @param part first xml-document
     * @param doc second xml-document
     * @return <code>true</code> iff all information in <code>part</code> 
     *         can also be found in <code>doc</code>
     */
    static public boolean contains(Document part, Document doc) {
        Collection<Element> remainingLeafs = new ArrayList<Element>();
        boolean c = contains(part.getDocumentElement(), doc.getDocumentElement(), remainingLeafs);
        dumpLeafs("remaining leafs", remainingLeafs);
        return c;
    }
    
    /**
     * Checks whether all information in a xml-document
     * can also be found in another document and vice versa.
     * 
     * @param a first xml-document
     * @param b second xml-document
     * @return <code>true</code> iff all information in <code>a</code> 
     *         can also be found in <code>b</code> and vice versa
     */
    static public boolean equals(Document a, Document b) {
        Collection<Element> remainingLeafsA = new ArrayList<Element>();
        Collection<Element> remainingLeafsB = new ArrayList<Element>();
        boolean c = contains(a.getDocumentElement(), b.getDocumentElement(), remainingLeafsA)
            && contains(b.getDocumentElement(), a.getDocumentElement(), remainingLeafsB);
        dumpLeafs("remaining leafs", remainingLeafsA);
        dumpLeafs("remaining leafs", remainingLeafsB);
        return c;
    }
    
    /**
     * Parses a XML-file.
     * 
     * @param xmlFile the XML file
     * @return the DOM-tree
     */
    public static Document parseXmlDocument(File xmlFile) throws ParserConfigurationException, FileNotFoundException, SAXException, IOException {
    	DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		DocumentBuilder builder = factory.newDocumentBuilder();
		Document document = builder.parse(new InputSource(new FileReader(xmlFile)));
		return document;
    }
    
    /**
     * Checks whether all information in a xml-element-tree
     * can also be found in another tree.
     * 
     * @param part first xml-element-tree
     * @param doc second xml-element-tree
     * @param remainingLeafs collection to put in all leafs from part which cannot be found in doc
     * @return <code>true</code> iff all information in <code>part</code> 
     *         can also be found in <code>doc</code>
     */
    static public boolean contains(Element part, Element doc, Collection<Element> remainingLeafs) {
        if (!part.getTagName().equals(doc.getTagName())) {
            remainingLeafs.addAll(leafs(part));
            return false;
        }
        if (part.getChildNodes().getLength() == 0) {
            String partText = part.getTextContent() == null? "" : part.getTextContent().trim();
            String docText = doc.getTextContent() == null? "" : doc.getTextContent().trim();
            if (!partText.equals(docText)) {
                remainingLeafs.add(part);
                return false;
            }
        }
        // TODO: check attributes too
        
        boolean docContainsPart = true;
        for (int i = 0; i < part.getChildNodes().getLength(); ++i) {
            if (!(part.getChildNodes().item(i) instanceof Element)) continue; 
        	Element partChild = (Element) part.getChildNodes().item(i);
            
            // search child in doc, child-order is not relevant
            Collection<Element> remainingLeafsOfAllChildren = null;
            boolean partChildFoundInDoc = false;
            for (int j = 0; j < doc.getChildNodes().getLength(); ++j) {
                if (!(doc.getChildNodes().item(j) instanceof Element)) continue; 
                Element docChild = (Element) doc.getChildNodes().item(j);
                Collection<Element> remainingLeafsOfChild = new ArrayList<Element>();
                if (contains(partChild, docChild, remainingLeafsOfChild)) {
                    partChildFoundInDoc = true;
                    break;
                }
                if (remainingLeafsOfAllChildren == null) {
                    remainingLeafsOfAllChildren = remainingLeafsOfChild;
                } else {
                    remainingLeafsOfAllChildren.retainAll(remainingLeafsOfChild);
                }
            }
            if (!partChildFoundInDoc) {
                if (remainingLeafsOfAllChildren != null) {
                    remainingLeafs.addAll(remainingLeafsOfAllChildren);
                } else {
                    System.out.print("");
                }
                docContainsPart = false;
            }
        }
        return docContainsPart;
    }

    /**
     * Collects all leafs of a element-tree.
     * 
     * @param root the root of the tree
     * @return all leafs of the element-tree
     */
    private static Collection<Element> leafs(Element root) {
        Collection<Element> leafs = new ArrayList<Element>();
        boolean isLeaf = true;
        for (int i = 0; i < root.getChildNodes().getLength(); ++i) {
        	if (root.getChildNodes().item(i) instanceof Element) {
	            Element child = (Element) root.getChildNodes().item(i);
	            leafs.addAll(leafs(child));
	            isLeaf = false;
        	}
        }
        if (isLeaf) {
            leafs.add(root);
        }
        return leafs;
    }
    
    /**
     * Dumps a set of leafs.
     * 
     * @param title the title
     * @param leafs the set of leafs
     */
    private static void dumpLeafs(String title, Collection<Element> leafs) {
        if (leafs.isEmpty()) {
            return;
        }
        System.out.println(title + ":");
        for (Iterator<Element> i = leafs.iterator(); i.hasNext(); ) {
            Element e = i.next();
            String path = "";
            for (Node n = e; n != null; n = n.getParentNode()) {
            	path = n.getNodeName() + "/" + path;
            }
            System.out.println(path + ":" + e.getTextContent());
        }
    }
}
