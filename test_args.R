args = commandArgs(trailingOnly=TRUE)

print(args[1])

# test
write.txt(args[1], file=paste(args[1],".txt"))
#