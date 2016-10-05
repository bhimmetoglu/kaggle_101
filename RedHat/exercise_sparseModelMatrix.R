# Burak Himmetoglu
# 
# Exercise: Sparse Model Matrices
#
require(Matrix)
dd <- data.frame(a = gl(3,4), b = gl(4,1,12))
str(dd)
mm <- model.matrix(~.-1, dd) # no bias term
dd$c = gl(3,2)
dd$c[sample(12,4)] <- NA # Add NA's
mm <- model.matrix(~.-1, dd) # This removes rows with NA

# Same thing for sparse.model.matrix
mm.sparse <- sparse.model.matrix(~.-1, dd)

# Suppose we add a numeric value
dd$d = rnorm(12)
mm <- model.matrix(~.-1, dd) # Works just fine
mm <- sparse.model.matrix(~.-1, dd) # Works just fine too

# Suppose we have a character column, that we want to use as factor eventually
types <- c("type_1", "type_2", "type_3")
dd$e <- rep(types,4); str(dd)

# Notice that there is an issue with _
#colnames(dd) <- c("col_1", "col_2", "col_3", "col_4", "col_5)
colnames(dd) <- c("c1", "c2", "c3", "c4", "c5")

mm <- model.matrix(~.-1, dd) # Does the conversion automatically
mm.sparse <- sparse.model.matrix(~.-1, dd) # Same..
