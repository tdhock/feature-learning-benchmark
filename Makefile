figure-auc-improved-interactive/index.html: figure-auc-improved-interactive.R auc.improved.rds
	R --vanilla < $<
auc.improved.rds: auc.improved.R
	R --vanilla < $<
target.intervals.features.csv:
	scp guillimin.hpc.mcgill.ca:PeakSegFPOP/target.intervals.features.csv .
