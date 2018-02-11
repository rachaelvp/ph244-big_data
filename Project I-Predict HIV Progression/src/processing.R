train <- read.csv("~/Desktop/training_data.csv")

train1 <- train %>%
  filter(Resp == 1)
nrow(train1)
# 206

train0 <- train %>%
  filter(Resp == 0)
nrow(train0)
# 794

#############################################################
# translate nucleic acid sequences into peptide sequence
#############################################################

# Even if two individuals have different DNA sequences for a gene,
# they can have the same protein sequences; and since only the protein
# is exposed to functional constraints, then it will be more interesting
# to see the differences in the protein sequences.

# PR - protease

train_PR <- tolower(train$PR.Seq) # lower case string
train_PR_s2c <- sapply(train_PR, s2c) # string to vector of characters
PR.AA <- sapply(train_PR_s2c[1:920],
  seqinr::translate, ambiguous = TRUE) # now I can convert to AA with the
                                       # 'seqinr' R package
PR.AA <- sapply(PR.AA, c2s) # converting the vector of characters to a string for each sequence
train$PR.AA <- c(PR.AA, rep(NA, 80)) # adding this column to the dataframe

# RT - reverse transcriptase
train_RT <- tolower(train$RT.Seq)
train_RT_s2c <- sapply(train_RT, s2c)
train$RT.AA <- sapply(train_RT_s2c, seqinr::translate, ambiguous = TRUE)
train$RT.AA <- sapply(train$RT.AA, c2s)

# make a new dataframe with the AA sequences added
# save it
write.csv(train, file = "~/Desktop/train.csv")

#############################################################
# allign nucleic acid sequences and peptide sequences
# then create a consensus matrix
#############################################################

# DNA Sequences alignment
# RT
ss_dna_RT <- DNAStringSet(train$RT.Seq) # first I have to convert the sequences
                                        # to a specific class, DNAStringSet
aln_RT.Seq <- muscle::muscle(ss_dna_RT) # then I can align them using the
                                        # 'muscle' R package on Bioconductor
write.csv(consensusMatrix(aln_RT.Seq),
file = "~/Desktop/consensusMatrix_RT.Seq.csv")
# PR
ss_dna_PR <- DNAStringSet(train$PR.Seq)
aln_PR.Seq <- muscle::muscle(ss_dna_PR)
write.csv(consensusMatrix(aln_PR.Seq),
file = "~/Desktop/consensusMatrix_PR.Seq.csv")

# AA Sequences alignment
# RT
ss_aa_RT <- AAStringSet(train$RT.AA) # first I have to convert the sequences
                                    # to a specific class, AAStringSet
aln_RT.AA <- msa(ss_aa_RT) # then I can align them using the 'msa' R package
                            # on Bioconductor
write.csv(consensusMatrix(aln_RT.AA),
file = "~/Desktop/consensusMatrix_RT.AA.csv")
#PR
ss_aa_PR <- AAStringSet(train$PR.AA[1:920])
aln_PR.AA <- msa::msa(ss_aa_PR)
write.csv(consensusMatrix(aln_PR.AA),
file = "~/Desktop/consensusMatrix_PR.AA.csv")

# make a new dataframe with the aligned sequences added
train_aln <- data.frame(train,
                     aln_PR.Seq = c(as.vector(DNAStringSet(aln_PR.Seq)),
                     rep(NA, 80)),
                     aln_RT.Seq = DNAStringSet(aln_RT.Seq),
                     aln_PR.AA = c(as.vector(AAStringSet(aln_PR.Seq)),
                     rep(NA, 80)),
                     aln_RT.AA = AAStringSet(aln_RT.AA))
# save it
write.csv(train_aln, file = "~/Desktop/train_aln.csv")

#############################################################
# compare the consensus matrices across binary outcome
#############################################################
# use probabilities for the consensus matrices
# bc we have unequal numbers in each group

# resp = 1, showed improvement
train1 <- train_aln %>%
  filter(Resp == 1)

PR.Seq_train1 <- consensusMatrix(DNAMultipleAlignment(train1$aln_PR.Seq[1:187]),
as.prob = TRUE) # used 1:187 bc the remaining rows are NA
RT.Seq_train1 <- consensusMatrix(DNAMultipleAlignment(train1$aln_RT.Seq),
as.prob = TRUE)
PR.AA_train1 <- consensusMatrix(AAMultipleAlignment(train1$aln_PR.AA[1:187]),
as.prob = TRUE)
PR.AA_train1 <- as.matrix(t(data.frame(t(PR.AA_train1),
                                        B = rep(0, 297),
                                        H = rep(0, 297),
                                        N = rep(0, 297),
                                        V = rep(0, 297)))) # such that the same
                                        # columns exist in PR.AA_train1 and
                                        # PR.AA_train0
RT.AA_train1 <- consensusMatrix(AAMultipleAlignment(train1$aln_RT.AA),
as.prob = TRUE)

# resp = 0, didn't show improvement
train0 <- train_aln %>%
  filter(Resp == 0)

PR.Seq_train0 <- consensusMatrix(DNAMultipleAlignment(train0$aln_PR.Seq[1:733]),
as.prob = TRUE) # used 1:733 bc the remaining rows are NA
RT.Seq_train0 <- consensusMatrix(DNAMultipleAlignment(train0$aln_RT.Seq),
as.prob = TRUE)
PR.AA_train0 <- consensusMatrix(AAMultipleAlignment(train0$aln_PR.AA[1:733]),
as.prob = TRUE)
RT.AA_train0 <- consensusMatrix(AAMultipleAlignment(train0$aln_RT.AA),
as.prob = TRUE)

# calculate a difference matrix
diff_PR.Seq <- t(PR.Seq_train1 - PR.Seq_train0)
write.csv(diff_PR.Seq, file = "~/Desktop/diff_PR.Seq.csv")
diff_RT.Seq <- t(RT.Seq_train1 - RT.Seq_train0)
write.csv(diff_RT.Seq, file = "~/Desktop/diff_RT.Seq.csv")
diff_PR.AA <- t(PR.AA_train1 - PR.AA_train0)
write.csv(diff_PR.AA, file = "~/Desktop/diff_PR.AA.csv")
diff_RT.AA <- t(RT.AA_train1 - RT.AA_train0)
write.csv(diff_RT.AA, file = "~/Desktop/diff_RT.AA.csv")

############################ plots

# PR.Seq
ggplot(data.frame(diff_PR.Seq), aes(y = rowSums(abs(data.frame(diff_PR.Seq))),
                        x = as.numeric(row.names(data.frame(diff_PR.Seq))))) +
         geom_point(cex=2, alpha = .6,
           aes(color=ifelse(rowSums(abs(data.frame(diff_PR.Seq)))>.2,
                                                  "#ff0000", "#ffffff"))) +
  theme(text = element_text(size=10),
      axis.text.x = element_text(angle=90, hjust=1),
      title = element_text(size = 12)) +
  theme(legend.position="none") +
  labs(x = "Aligned Position Index",
  y= "Likelihood of Differential Expression") +
  ggtitle("Differentially Expressed Positions within Protease DNA Sequence")

# RT.Seq
ggplot(data.frame(diff_RT.Seq), aes(y = rowSums(abs(data.frame(diff_RT.Seq))),
                        x = as.numeric(row.names(data.frame(diff_RT.Seq))))) +
  geom_point(cex=2, alpha = .6,
    aes(color=ifelse(rowSums(abs(data.frame(diff_RT.Seq)))>.4,
                                                 "#ff0000", "black"))) +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1),
        title = element_text(size = 12)) +
  theme(legend.position="none") +
  labs(x = "Aligned Position Index",
  y= "Likelihood of Differential Expression") +
  ggtitle("Differentially Expressed Positions within
  Reverse Transcriptase DNA Sequence")

# PR.AA
ggplot(data.frame(diff_PR.AA), aes(y = rowSums(abs(data.frame(diff_PR.AA)))/2,
                          x = as.numeric(row.names(data.frame(diff_PR.AA))))) +
  geom_point(cex=2, alpha = .6,
    aes(color=ifelse(rowSums(abs(data.frame(diff_PR.AA)))>1,
                                                 "#ff0000", "black"))) +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1),
        title = element_text(size = 12)) +
  theme(legend.position="none") +
  labs(x = "Aligned Position Index",
  y= "Likelihood of Differential Translation") +
  ggtitle("Differentially Translated Positions within Protease AA Sequence")

# RT.AA
ggplot(data.frame(diff_RT.AA), aes(y = rowSums(abs(data.frame(diff_RT.AA))),
                        x = as.numeric(row.names(data.frame(diff_RT.AA))))) +
  geom_point(cex=2, alpha = .6,
    aes(color=ifelse(rowSums(abs(data.frame(diff_RT.AA)))>.2,
                                                 "#ff0000", "black"))) +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1),
        title = element_text(size = 12)) +
  theme(legend.position="none") +
  labs(x = "Aligned Position Index",
  y= "Likelihood of Differential Translation") +
  ggtitle("Differentially Translated Positions within
  Reverse Transcriptase AA Sequence")

#############################################################
# identify the top 20 sites that show evidence of SNPs/SAPs
#############################################################

########################### DNA Sequences

# PR.Seq
abs_diff_PR.Seq <- data.frame(abs.sum = rowSums(abs(data.frame(diff_PR.Seq))),
                    position = as.numeric(row.names(data.frame(diff_PR.Seq))))
sort_diff_PR.Seq <- abs_diff_PR.Seq[order(abs_diff_PR.Seq$abs.sum,
  decreasing = TRUE),]
head(sort_diff_PR.Seq,10)$position
# [1]  28   6 245 212 188 268 160 138 277 225

aln_PR.Seq <- as.matrix(DNAStringSet(train_aln$aln_PR.Seq[1:920]))
top_PR.Seq <- data.frame(aln_PR.Seq[,c(28,6,245,212,188,268,160,138,277,225)])
colnames(top_PR.Seq) <- c(28,6,245,212,188,268,160,138,277,225)
top_PR.Seq[nrow(top_PR.Seq)+80,] <- NA

# RT.Seq
abs_diff_RT.Seq <- data.frame(abs.sum = rowSums(abs(data.frame(diff_RT.Seq))),
                    position = as.numeric(row.names(data.frame(diff_RT.Seq))))
sort_diff_RT.Seq <- abs_diff_RT.Seq[order(abs_diff_RT.Seq$abs.sum,
  decreasing = TRUE),]
head(sort_diff_RT.Seq,10)$position
# [1]  972 1224  951 1308  942  550  939 1299 1169 1317

aln_RT.Seq <- as.matrix(DNAStringSet(train_aln$aln_RT.Seq))
top_RT.Seq <- data.frame(aln_RT.Seq[,
  c(972,1224,951,1308,942,550,939,1299,1169,1317)])
colnames(top_RT.Seq) <- c(972,1224,951,1308,942,550,939,1299,1169,1317)

# put it all together
Seq <- data.frame(Resp = train_aln$Resp, top_PR.Seq, top_RT.Seq,
                  VL.t0 = train_aln$VL.t0, CD4.t0 = train_aln$CD4.t0)
colnames(Seq) <- c("Resp","PR.28","PR.6","PR.245","PR.212","PR.188","PR.268",
"PR.160","PR.138","PR.277","PR.225","RT.972","RT.1224","RT.951","RT.1308",
"RT.942","RT.550","RT.939","RT.1299","RT.1169","RT.1317","VL.t0","CD4.t0")
write.csv(Seq, "~/Desktop/Seq.csv")

########################### AA Sequences

# PR.AA
abs_diff_PR.AA <- data.frame(abs.sum = rowSums(abs(data.frame(diff_PR.AA))),
                    position = as.numeric(row.names(data.frame(diff_PR.AA))))
sort_diff_PR.AA <- abs_diff_PR.AA[order(abs_diff_PR.AA$abs.sum,
  decreasing = TRUE),]
head(sort_diff_PR.AA,10)$position
# [1]  46 49 52 61 64 65 68 71 73 75

aln_PR.AA <- as.matrix(AAStringSet(train_aln$aln_PR.AA[1:920]))
top_PR.AA <- data.frame(aln_PR.AA[,c(46,49,52,61,64,65,68,71,73,75)])
colnames(top_PR.AA) <- c(46,49,52,61,64,65,68,71,73,75)
top_PR.AA[nrow(top_PR.AA)+80,] <- NA

# RT.AA
abs_diff_RT.AA <- data.frame(abs.sum = rowSums(abs(data.frame(diff_RT.AA))),
                    position = as.numeric(row.names(data.frame(diff_RT.AA))))
sort_diff_RT.AA <- abs_diff_RT.AA[order(abs_diff_RT.AA$abs.sum,
  decreasing = TRUE),]
head(sort_diff_RT.AA,10)$position
# [1]  41 210 215 118  43 122 219  44  70 211

aln_RT.AA <- as.matrix(AAStringSet(train_aln$aln_RT.AA))
top_RT.AA <- data.frame(aln_RT.AA[,c(41,210,215,118,43,122,219,44,70,211)])
colnames(top_RT.AA) <- c(41,210,215,118,43,122,219,44,70,211)

# put it all together
AA <- data.frame(Resp = train_aln$Resp, top_PR.AA, top_RT.AA,
                  VL.t0 = train_aln$VL.t0, CD4.t0 = train_aln$CD4.t0)
colnames(AA) <- c("Resp","PR.46","PR.49","PR.52","PR.61","PR.64","PR.65",
"PR.68","PR.71","PR.73","PR.75", "RT.41", "RT.210","RT.215","RT.118","RT.43",
"RT.122","RT.219","RT.44","RT.70","RT.211", "VL.t0","CD4.t0")
write.csv(AA, "~/Desktop/AA.csv")
