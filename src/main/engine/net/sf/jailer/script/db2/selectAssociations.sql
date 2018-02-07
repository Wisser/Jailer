SELECT TABNAME, REFTABNAME, ''B'', ''n:1'', ''A.'' || rtrim(ltrim(FK_COLNAMES)) || ''='' || ''B.'' || rtrim(ltrim(PK_COLNAMES))
FROM syscat.REFERENCES 
WHERE TABSCHEMA=UPPER(''{0}'') ORDER BY TABNAME
