
docs <- fread('data/v3form.csv', header=T)
tags = unique(trimws(unlist(strsplit(docs$'2.1. What type of flow is it?', split=", "))))
for(tag in tags) {
  set(docs, j=tag, value=F)
  set(docs, i=grep(tag, docs$'2.1. What type of flow is it?'), j=tag, value=T)
}
write.csv(docs, 'data/001_output_flowtypes.csv')
