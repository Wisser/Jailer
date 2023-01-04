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
package net.sf.jailer.datamodel.filter_template;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Quoting;


/**
 * A clause of a {@link FilterTemplate}.
 * 
 * @author Ralf Wisser
 */
public class Clause {

	/**
	 * Subjects.
	 */
	public enum Subject {
		COLUMN("Column", Boolean.class) { // PK?
			@Override
			public Object getSubject(Table table, Column column) {
				for (Column pk: table.primaryKey.getColumns()) {
					if (column.equals(pk)) {
						return true;
					}
				}
				return false;
			}
		},
		COLUMN_NAME("Column name", String.class) {
			@Override
			public Object getSubject(Table table, Column column) {
				return Quoting.staticUnquote(column.name);
			}
		},
		TABLE_NAME("Table name", String.class) {
			@Override
			public Object getSubject(Table table, Column column) {
				String schema = table.getOriginalSchema("");
				if (schema.length() == 0) {
					return Quoting.staticUnquote(table.getUnqualifiedName());
				}
				return Quoting.staticUnquote(schema) + "." + Quoting.staticUnquote(table.getUnqualifiedName());
			}
		},
		TYPE("Type", String.class) {
			@Override
			public Object getSubject(Table table, Column column) {
				return column.type;
			}
		};
		
		public final String description;
		public final Class<?> type;
		
		private Subject(String description, Class<?> type) {
			this.description = description;
			this.type = type;
		}
		
		public abstract Object getSubject(Table table, Column column);
		
		@Override
		public String toString() {
			return description;
		}
	}

	/**
	 * Predicates.
	 */
	public enum Predicate {
		EQUALS("=", String.class, true) {
			@Override
			protected boolean eval(Object subject, String object) {
				return object.equalsIgnoreCase((String) subject);
			}
		},
		NOT_EQUALS("<>", String.class, true) {
			@Override
			protected boolean eval(Object subject, String object) {
				return !object.equalsIgnoreCase((String) subject);
			}
		},
		
		STARTS_WITH("starts with", String.class, true) {
			@Override
			protected boolean eval(Object subject, String object) {
				return subject.toString().toLowerCase(Locale.ENGLISH).startsWith(object.toLowerCase(Locale.ENGLISH));
			}
		},
		NOT_STARTS_WITH("not starts with", String.class, true) {
			@Override
			protected boolean eval(Object subject, String object) {
				return !subject.toString().toLowerCase(Locale.ENGLISH).startsWith(object.toLowerCase(Locale.ENGLISH));
			}
		},
		ENDS_WITH("ends with", String.class, true) {
			@Override
			protected boolean eval(Object subject, String object) {
				return subject.toString().toLowerCase(Locale.ENGLISH).endsWith(object.toLowerCase(Locale.ENGLISH));
			}
		},
		NOT_ENDS_WITH("not ends with", String.class, true) {
			@Override
			protected boolean eval(Object subject, String object) {
				return !subject.toString().toLowerCase(Locale.ENGLISH).endsWith(object.toLowerCase(Locale.ENGLISH));
			}
		},
		CONTAINS("contains", String.class, true) {
			@Override
			protected boolean eval(Object subject, String object) {
				return subject.toString().toLowerCase(Locale.ENGLISH).contains(object.toLowerCase(Locale.ENGLISH));
			}
		},
		NOT_CONTAINS("not contains", String.class, true) {
			@Override
			protected boolean eval(Object subject, String object) {
				return !subject.toString().toLowerCase(Locale.ENGLISH).contains(object.toLowerCase(Locale.ENGLISH));
			}
		},
		
		MATCHES("matches", String.class, true) {
			@Override
			protected boolean eval(Object subject, String object) {
				return matches((String) subject, object);
			}
			@Override
			public String validateObject(String object) {
				return validateRE(object);
			}
		},
		NOT_MATCHES("not matches", String.class, true) {
			@Override
			protected boolean eval(Object subject, String object) {
				return !matches((String) subject, object);
			}
			@Override
			public String validateObject(String object) {
				return validateRE(object);
			}
		},
		
		LIKE("like", String.class, true) {
			@Override
			protected boolean eval(Object subject, String object) {
				return like((String) subject, object);
			}
		},
		NOT_LIKE("not like", String.class, true) {
			@Override
			protected boolean eval(Object subject, String object) {
				return !like((String) subject, object);
			}
		},
		
		IS_PK("is primary key", Boolean.class, false) {
			@Override
			protected boolean eval(Object subject, String object) {
				return Boolean.TRUE.equals(subject);
			}
		},
		IS_NOT_PK("is not primary key", Boolean.class, false) {
			@Override
			protected boolean eval(Object subject, String object) {
				return Boolean.FALSE.equals(subject);
			}
		};
		
		public final String description;
		public final Class<?> type;
		public final boolean needsObject;
		
		private Predicate(String description, Class<?> type, boolean needsObject) {
			this.description = description;
			this.type = type;
			this.needsObject = needsObject;
		}
		
		protected boolean like(String subject, String pattern) {
			return matches(subject, Pattern.quote(pattern).replace("_", "\\E.\\Q").replace("%", "\\E.*\\Q"));
		}

		private static Map<String, Pattern> compiledPattern = new HashMap<String, Pattern>();
		
		protected synchronized boolean matches(String subject, String pattern) {
			try {
				Pattern pat = compiledPattern.get(pattern);
				if (pat == null) {
					pat = Pattern.compile(pattern, Pattern.CASE_INSENSITIVE);
					compiledPattern.put(pattern, pat);
				}
				return pat.matcher(subject).matches();
			} catch (Exception e) {
				return false;
			}
		}

		protected String validateRE(String pattern) {
			try {
				Pattern.compile(pattern, Pattern.CASE_INSENSITIVE);
				return null;
			} catch (PatternSyntaxException e) {
				return "Invalid regular expression:\n" + e.getMessage();
			}
		}

		protected abstract boolean eval(Object subject, String object);
		
		/**
		 * Validates an object.
		 * 
		 * @param object the object
		 * @return <code>null</code> if object is valid, otherwise a clear-text description of the problem
		 */
		public String validateObject(String object) {
			return null;
		}

		@Override
		public String toString() {
			return description;
		}
	}

	/**
	 * The subject.
	 */
	private Subject subject = Subject.COLUMN_NAME;
	
	/**
	 * The predicate.
	 */
	private Predicate predicate = Predicate.EQUALS;
	
	/**
	 * The object.
	 */
	private String object = "";

	/**
	 * Constructor.
	 */
	public Clause() {
	}

	/**
	 * Copy constructor.
	 */
	public Clause(Clause other) {
		this.subject = other.subject;
		this.predicate = other.predicate;
		this.object = other.object;
	}

	/**
	 * @return the subject
	 */
	public Subject getSubject() {
		return subject;
	}

	/**
	 * @param subject the subject to set
	 */
	public void setSubject(Subject subject) {
		this.subject = subject;
	}

	/**
	 * @return the predicate
	 */
	public Predicate getPredicate() {
		return predicate;
	}

	/**
	 * @param predicate the predicate to set
	 */
	public void setPredicate(Predicate predicate) {
		this.predicate = predicate;
	}

	/**
	 * @return the object
	 */
	public String getObject() {
		return object;
	}

	/**
	 * @param object the object to set
	 */
	public void setObject(String object) {
		this.object = object;
	}
	
	/**
	 * Evaluates the clause for a given column.
	 * 
	 * @param table the column's table 
	 * @param column the column
	 * @return result
	 */
	public boolean eval(Table table, Column column) {
		return predicate.eval(subject.getSubject(table, column), object);
	}
	
}
