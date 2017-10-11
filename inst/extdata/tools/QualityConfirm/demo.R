source(file = "./ClassDefine.R")

Obj <- new("ReadsCheck", list(panelBed = "./example/APL_panel.bed", bamDir = "./example"))

# 1. Merge the panel
Obj <- PanelMerge(Obj)
Obj <- PlotBedHist(Obj)

# 2. Deal with each bam file
for (loop in c(1:length(Obj@bamList))) {
  print(loop)
  if (Obj@status == 0) {
    Obj <- ReadsDepth(Obj, test = T)
  }
  if (Obj@status == 1) {
    Obj <- PlotCountFig(Obj)
  }
  if (Obj@status == 3) {
    Obj <- SingleReadsSummary(Obj)
  }
  if (Obj@status == 7) {
    Obj@status = 0
  }
}
if (Obj@currBamNum == length(Obj@bamList) + 1) {
  Obj <- AllReadsSummary(Obj)
}

# Obj <- ReadsDepth(Obj, test = T) Obj <- PanelMerge(Obj) Obj <- PlotCountFig(Obj) Obj <-
# PlotBedHist(Obj) Obj <- SingleReadsSummary(Obj) Obj <- AllReadsSummary(Obj)
