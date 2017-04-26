${for-each:comments}

-- Create import-filter mapping table (JAILER_IFM)
${end}
-- try: ${drop-table}${schema}${mapping-table};
${create-table}${schema}${mapping-table}
(
${old-and-new-values}
) ${create-table-suffix};

${for-each:indexed-columns}${create-index}${index-schema}${mapping-table}_I$i ON ${index-table-prefix}${schema}${mapping-table}($) ${create-index-suffix};
${end}