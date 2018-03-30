write("", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex",append=FALSE)
resultDirectory<-"E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/data"
latexHeader <- function() {
  write("\\documentclass{article}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\title{StandardStudy}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\usepackage{amssymb}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\author{A.J.Nebro}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\begin{document}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\maketitle", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\section{Tables}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
}

latexTableHeader <- function(problem, tabularString, latexTableFirstLine) {
  write("\\begin{table}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\caption{", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write(problem, "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write(".HV.}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)

  write("\\label{Table:", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write(problem, "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write(".HV.}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)

  write("\\centering", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\begin{scriptsize}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\begin{tabular}{", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write(tabularString, "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write(latexTableFirstLine, "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\hline ", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
}

latexTableTail <- function() { 
  write("\\hline", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\end{tabular}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\end{scriptsize}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  write("\\end{table}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
}

latexTail <- function() { 
  write("\\end{document}", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
}

printTableLine <- function(indicator, algorithm1, algorithm2, i, j, problem) { 
  file1<-paste(resultDirectory, algorithm1, sep="/")
  file1<-paste(file1, problem, sep="/")
  file1<-paste(file1, indicator, sep="/")
  data1<-scan(file1)
  file2<-paste(resultDirectory, algorithm2, sep="/")
  file2<-paste(file2, problem, sep="/")
  file2<-paste(file2, indicator, sep="/")
  data2<-scan(file2)
  if (i == j) {
    write("--", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  }
  else if (i < j) {
    if (wilcox.test(data1, data2)$p.value <= 0.05) {
      if (median(data1) >= median(data2)) {
        write("$\\blacktriangle$", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
      }
      else {
        write("$\\triangledown$", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE) 
      }
    }
    else {
      write("--", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE) 
    }
  }
  else {
    write(" ", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
  }
}

### START OF SCRIPT 
# Constants
problemList <-c("ZDT1", "ZDT2", "ZDT3") 
algorithmList <-c("NSGAII", "SPEA2") 
tabularString <-c("lc") 
latexTableFirstLine <-c("\\hline  & SPEA2\\\\ ") 
indicator<-"HV"

 # Step 1.  Writes the latex header
latexHeader()
# Step 2. Problem loop 
for (problem in problemList) {
  latexTableHeader(problem,  tabularString, latexTableFirstLine)

  indx = 0
  for (i in algorithmList) {
    if (i != "SPEA2") {
      write(i , "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
      write(" & ", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
      jndx = 0 
      for (j in algorithmList) {
        if (jndx != 0) {
          if (indx != jndx) {
            printTableLine(indicator, i, j, indx, jndx, problem)
          }
          else {
            write("  ", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
          }
          if (j != "SPEA2") {
            write(" & ", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
          }
          else {
            write(" \\\\ ", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
          }
        }
        jndx = jndx + 1
      }
      indx = indx + 1
    }
  }

  latexTableTail()
} # for problem

tabularString <-c("| l | p{0.15cm}  p{0.15cm}  p{0.15cm}   | ") 

latexTableFirstLine <-c("\\hline \\multicolumn{1}{|c|}{} & \\multicolumn{3}{c|}{SPEA2} \\\\") 

# Step 3. Problem loop 
latexTableHeader("ZDT1 ZDT2 ZDT3 ", tabularString, latexTableFirstLine)

indx = 0
for (i in algorithmList) {
  if (i != "SPEA2") {
    write(i , "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
    write(" & ", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)

    jndx = 0
    for (j in algorithmList) {
      for (problem in problemList) {
        if (jndx != 0) {
          if (i != j) {
            printTableLine(indicator, i, j, indx, jndx, problem)
          }
          else {
            write("  ", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
          } 
          if (problem == "ZDT3") {
            if (j == "SPEA2") {
              write(" \\\\ ", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
            } 
            else {
              write(" & ", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
            }
          }
     else {
    write("&", "E:\eclipse-workspace\Dome_Jmetal\src\jmetalStandardStudy/R/ZDT.HV.Wilcox.tex", append=TRUE)
     }
        }
      }
      jndx = jndx + 1
    }
    indx = indx + 1
  }
} # for algorithm

  latexTableTail()

#Step 3. Writes the end of latex file 
latexTail()

