library(tcltk)
library(tcltk2)
library(RColorBrewer)


## Make a grid of radio buttons to select from a range of options that
## are common to many items. User must supply
##
## parent - the parent window
## label - the label for the enclosing frame
## rows - the labels for the rows/items
## choices - the labels for the columns/choices
## initial - the initial selection
##
##
radiogrid <- function(parent,label,rows,choices,initial=1,label.rows=T) {
  if(is.null(names(choices))) names(choices) <- as.character(choices)
  initial <- rep(initial,length=length(rows))
  state <- lapply(initial,function(k) tclVar(names(choices)[k]))
  names(state) <- rows
  tk.frame <- tk2labelframe(parent,text=label)
  for(col in seq_along(choices))
    tkgrid(tk2label(tk.frame,text=names(choices)[col]),row=0,column=col)
  for(row in seq_along(rows)) {
    tkgrid(tk2label(tk.frame,text=if(label.rows) rows[row] else ""),
                    row=row,column=0,sticky="w")
    for(col in seq_along(choices))
      tkgrid(tk2radiobutton(tk.frame,value=names(choices)[col],variable=state[[row]]),row=row,column=col)
  }
  r <- list(window=tk.frame,
            selected=function() {
              r <- choices[sapply(state,tclvalue)]
              names(r) <- rows
              r
            },
            state=state,choices=choices)
  class(r) <- "radiogrid"
  r
}


## Make a grid of check buttons to select elements of the adjacency matrix.
## are common to many items. User must supply
##
## parent - the parent window
## label - the label for the enclosing frame
## rows - the labels for the rows (node names)
## edges - an nx2 matrix that defines the edges
## group - a numeric vector that groups edges
checkedges <- function(parent,label,rows,edges,group=NULL,label.rows=T) {
  group <- if(is.null(group)) seq_len(nrow(edges)) else match(group,unique(group))
  state <- lapply(unique(group),function(k) tclVar("0"))[group]
  tk.frame <- tk2labelframe(parent,text=label)
  for(k in seq_along(rows)) {
    tkgrid(tk2label(tk.frame,text=if(label.rows) rows[k] else ""),row=k,column=0,sticky="w")
    tkgrid(tk2label(tk.frame,text=k),row=k,column=1)
    tkgrid(tk2label(tk.frame,text=k),row=0,column=k+1)
  }
  for(k in seq_len(nrow(edges)))
    tkgrid(tk2checkbutton(tk.frame,text="",variable=state[[k]]),row=edges[k,2],column=edges[k,1]+1)

  r <- list(window=tk.frame,
            selected=function() sapply(state,tclvalue)=="1",
            state=state)
  class(r) <- "checkedges"
  r
}




## Make a column of checkboxes. User must supply
##
## parent - the parent window
## label - the label for the enclosing frame
## rows - the labels for the rows/items
checkcolumn <- function(parent,label,rows,label.rows=T) {
  state <- lapply(rows,function(k) tclVar("0"))
  names(state) <- rows
  tk.frame <- tk2labelframe(parent,text=label)
  for(row in seq_along(rows))
    tkgrid(tk2checkbutton(tk.frame,text=if(label.rows) rows[[row]] else "",
                          variable=state[[row]]),row=row-1,column=0,sticky="w")
  r <- list(window=tk.frame,
            selected=function() sapply(state,tclvalue)=="1",
            state=state)
  class(r) <- "checkcolumn"
  r
}


## Make a column of radiobuttons. User must supply
##
## parent - the parent window
## label - the label for the enclosing frame
## choices - the labels for the rows/choices
## initial - the index of the initial choice
radiocolumn <- function(parent,label,choices,initial=1,label.rows=T) {
  if(is.null(names(choices))) names(choices) <- as.character(choices)
  state <- tclVar(names(choices)[initial])
  tk.frame <- tk2labelframe(parent,text=label)
  for(row in seq_along(choices))
    tkgrid(tk2radiobutton(tk.frame,text=if(label.rows) choices[[row]] else "",
                          value=names(choices)[row],variable=state),row=row-1,column=0,sticky="w")
  r <- list(window=tk.frame,
            selected=function() tclvalue(state),
            state=state)
  class(r) <- "radiocolumn"
  r
}







## Make a single checkbox. User must supply
##
## parent - the parent window
## label - the label for the enclosing frame
## rows - the labels for the rows/items
checkbox <- function(parent,label,initial=0) {
  state <- tclVar(initial)
  w.check <- tk2checkbutton(parent,text=label,variable=state)
  r <- list(window=w.check,
            selected=function() tclvalue(state)=="1",
            state=state)
  class(r) <- "checkbox"
  r
}


## Make a slider.
##
## parent - the parent window
## initial - initial value of the slider
## from - minimum value
## to - maximum value
## orient - horizontal or vertical
slider <- function(parent,initial=1,from=0,to=100,orient="horizontal") {
  state <- tclVar(initial)
  w.slider <- tk2scale(parent,orient=orient,from=to,to=from,variable=state)
  r <- list(window=w.slider,
            selected=function() as.numeric(tclvalue(state)),
            state=state)
  class(r) <- "slider"
  r
}


## Make a slider.
##
## parent - the parent window
## initial - initial value of the slider
## from - minimum value
## to - maximum value
## orient - horizontal or vertical
slidercolumn <- function(parent,label,initial=1,from=0,to=100) {
  state <- tclVar(initial)
  tk.frame <- tk2labelframe(parent,text=label)
  tkgrid(tk2scale(tk.frame,orient="vertical",from=from,to=to,variable=state),padx=2,pady=2)
  r <- list(window=tk.frame,
            selected=function() as.numeric(tclvalue(state)),
            state=state)
  class(r) <- "slider"
  r
}





## Interactively select nodes to perturb/monitor, from a subset of
## models, and then perform a given action.
interactive.selection <- function(action,nodes,edges=NULL,
                                  slider=NULL,checkbox=NULL,
                                  perturb=T,monitor=T) {

  tk.top <- tktoplevel()
  tktitle(tk.top) <- "Node Selector"
  label <- T
  w.perturb <- if(perturb) radiogrid(tk.top,"Perturb",nodes,c("-"=-1,"0"=0,"+"=1),initial=2,label.rows=label && !(label <- F))
  w.monitor <- if(monitor) radiogrid(tk.top,"Monitor",nodes,c("-"=-1,"0"=0,"+"=1,"?"=NA),initial=4,label.rows=label && !(label <- F))
  w.edges <- if(!is.null(edges)) checkedges(tk.top,"Edges",nodes,edges,label.rows=label && !(label <- F))
  w.checkbox <- if(!is.null(checkbox)) checkbox(tk.top,checkbox,0)
  w.slider <- if(!is.null(slider)) slider(tk.top,slider$initial,slider$to,slider$from)

  update <- function() {
    action(perturb=if(!is.null(w.perturb)) w.perturb$selected(),
           monitor=if(!is.null(w.monitor)) w.monitor$selected(),
           edges=if(!is.null(w.edges)) w.edges$selected(),
           check=if(!is.null(w.checkbox)) w.checkbox$selected(),
           slider=if(!is.null(w.slider)) w.slider$selected())
    Sys.sleep(0.1)
    tkfocus(tk.top)
  }

  close <- function() {
    tkdestroy(tk.top)
  }

  col <- -1
  if(!is.null(w.perturb)) tkgrid(w.perturb$window,padx=2,pady=2,row=0,column=(col <- col+1),sticky="n")
  if(!is.null(w.monitor)) tkgrid(w.monitor$window,padx=2,pady=2,row=0,column=(col <- col+1),sticky="n")
  if(!is.null(w.edges)) tkgrid(w.edges$window,padx=2,pady=2,row=0,column=(col <- col+1),sticky="n")
  tk.frame <- tkframe(tk.top)

  tkgrid(tk2button(tk.frame,text="Update",command=update),
         tk2button(tk.frame,text="Close",command=close))
  tkgrid(tk.frame,
         if(!is.null(w.checkbox)) w.checkbox$window,
         if(!is.null(w.slider)) w.slider$window)

  tkfocus(tk.top)
}




impact.barplot <- function(edges,As,epsilon=1.0E-5) {
  pal <- c(brewer.pal(n=7,"RdYlGn")[2],brewer.pal(n=7,"RdYlGn")[4],brewer.pal(n=7,"RdYlGn")[6])
  nodes <- node.labels(edges)


  action <- function(perturb,monitor,edges,check,slider) {
    results <- 0
    for(i in 1:length(As)) {
      impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
      if(all(monitor==impact,na.rm=T)) {
        results <- results + outer(impact,-1:1,'==')
      }
    }
    if(length(results)>0) {
      plot.new()
      prop <- results/length(As)
      rownames(prop) <- nodes
      lwidth <- max(strwidth(nodes,"inches"))
      opar <- par(mai=c(1,lwidth+0.4,0.4,0.4)+0.2)
      barplot(t(prop),horiz=T,las=1,cex.names=1.2,cex.axis=1.2,cex.lab=1.2,border=F,col=pal,xlab="Proportion")
      par(opar)
      print(prop)
    }
  }

  interactive.selection(action,nodes,
                        perturb=T,monitor=T)
}


weight.pca <- function(edges,As,ws,epsilon=1.0E-5,cex=0.7) {
  pal <- paste(brewer.pal(n=5,"RdBu")[c(1,5)])
  nodes <- node.labels(edges)
  colnames(ws) <- paste(paste("T",unclass(edges$To),sep=""),
                        paste("F",unclass(edges$From),sep=""),sep="")

  action <- function(perturb,monitor,edges,check,slider) {
    if(any(edges)) {
      keep <- rep(F,nrow(ws))
      for(i in 1:length(As)) {
        impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
        if(all(monitor==impact,na.rm=T)) keep[i] <- T
      }
      if(check) {
        pr <- prcomp(ws[keep,edges])
        print(pr)
        biplot(pr,
               xlabs=rep("+",sum(keep)),
               ylabs=colnames(ws)[edges],
               cex=cex)
      } else {
        pr <- prcomp(ws[,edges])
        print(pr)
        biplot(pr,
               xlabs=ifelse(keep,"+","."),
               ylabs=colnames(ws)[edges],
               cex=cex)
      }
    }
  }

  interactive.selection(action,nodes,cbind(edges$From,edges$To),
                        checkbox="subset",perturb=T,monitor=T)
}




weight.pairs <- function(edges,As,ws,epsilon=1.0E-5,pch=16,...) {
  pal <- brewer.pal(n=5,"RdBu")[c(1,5)]
  nodes <- node.labels(edges)
  colnames(ws) <- paste(unclass(edges$To),unclass(edges$From),sep=":")
  dots <- list(...)

  wpairs <- function(x,col) {
    do.call("pairs",c(list(x=x,col=col,pch=pch),dots))
  }


  action <- function(perturb,monitor,edges,check,slider) {
    print(slider)
    if(any(edges)) {
      keep <- rep(F,nrow(ws))
      for(i in 1:length(As)) {
        impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
        if(all(monitor==impact,na.rm=T)) keep[i] <- T
      }
      k <- if(sum(keep)<sum(!keep)) order(keep) else order(-keep)
      shade <- paste(pal,as.hexmode(floor(slider)),sep="")
      print(shade)
      wpairs(x=ws[k,edges],col=shade[keep[k]+1])
    }
  }

  interactive.selection(action,nodes,cbind(edges$From,edges$To),
                        slider=list(initial=255,from=10,to=255),perturb=T,monitor=T)
}


weight.density <- function(edges,As,ws,epsilon=1.0E-5) {
  pal <- brewer.pal(n=5,"RdBu")[c(1,5)]
  nodes <- node.labels(edges)
  #colnames(ws) <- paste(unclass(edges$To),unclass(edges$From),sep=":")
  colnames(ws) <- paste(edges$From,edges$To,sep=" : ")

  action <- function(perturb,monitor,edges,check,slider) {
    if(any(edges)) {
      keep <- rep(F,nrow(ws))
      for(i in 1:length(As)) {
        impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
        if(all(monitor==impact,na.rm=T)) keep[i] <- T
      }
      n <- ceiling(sqrt(sum(edges)))
      m <- ceiling(sum(edges)/n)

      opar <- par(mfrow=c(m,n),mar=c(5,4,1,1)+0.1)
      for(k in which(edges)) {
        d1 <- density(ws[keep,k],adjust=slider)
        d2 <- density(ws[!keep,k],adjust=slider)
        plot(d1,xlab=colnames(ws)[k],main="",
        #     xlim=range(d1$x,d2$x),ylim=range(d1$y,d2$y),col=pal[1])
        #lines(d2,col=pal[2])
             xlim=range(d1$x,d2$x),ylim=range(d1$y,d2$y))
        lines(d2,lty=2)
      }
      par(opar)
    }
  }

  interactive.selection(action,nodes,cbind(edges$From,edges$To),
                        slider=list(initial=1,from=0,to=2),perturb=T,monitor=T)
}


weight.spairs <- function(edges,As,ws,epsilon=1.0E-5,nrpoints=0,...) {
  nodes <- node.labels(edges)
  colnames(ws) <- paste(unclass(edges$To),unclass(edges$From),sep=":")
  dots <- list(...)

  spairs <- function(x,bw) {
    do.call("pairs",
            c(list(x=x,panel = function(...)
                   smoothScatter(...,nrpoints=nrpoints,bandwidth=bw,add=TRUE)),
              dots))
  }


  action <- function(perturb,monitor,edges,check,slider) {
    if(any(edges)) {
      keep <- rep(F,nrow(ws))
      for(i in 1:length(As)) {
        impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
        if(all(monitor==impact,na.rm=T)) keep[i] <- T
      }
      spairs(ws[keep,edges],bw=slider)
    }
  }

  interactive.selection(action,nodes,cbind(edges$From,edges$To),
                        slider=list(initial=0.1,from=0,to=0.3),perturb=T,monitor=T)
}



library(MASS)
weight.discrim <- function(edges,As,ws,epsilon=1.0E-5,...) {
  nodes <- node.labels(edges)
  colnames(ws) <- paste(unclass(edges$To),unclass(edges$From),sep=":")

  action <- function(perturb,monitor,edges,check,slider) {
    if(any(edges)) {
      keep <- rep(F,nrow(ws))
      for(i in 1:length(As)) {
        impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
        if(all(monitor==impact,na.rm=T)) keep[i] <- T
      }
      group <- ifelse(keep,"Y","N")
      fit <- if(check) qda(x=ws[,edges],group=group) else lda(x=ws[,edges],group=group)
      print(fit)
      print(table(group,predict(fit)$class))
    }
  }

  interactive.selection(action,nodes,cbind(edges$From,edges$To),
                        checkbox="Quadratic",perturb=T,monitor=T)
}



weight.bglm <- function(edges,As,ws,epsilon=1.0E-5) {
  pal <- paste(brewer.pal(n=5,"RdBu")[c(1,5)])
  nodes <- node.labels(edges)
  colnames(ws) <- paste(paste("T",unclass(edges$To),sep=""),
                        paste("F",unclass(edges$From),sep=""),sep="")

  poly <- function(vs,quadratic=T) {
    if(quadratic) {
      paste("y~",
            paste(vs,collapse="+"),
            "+",
            paste("I(",vs,"^2)",sep="",collapse="+"),
            "+I(",
            do.call(paste,
                    c(as.data.frame(t(combn(vs, 2))),sep="*",collapse=")+I(")),
            ")",sep="")
    } else {
      paste("y~",paste(vs,collapse="+"),sep="")
    }
  }


  action <- function(perturb,monitor,edges,check,slider) {
    if(any(edges)) {
      keep <- rep(F,nrow(ws))
      for(i in 1:length(As)) {
        impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
        if(all(monitor==impact,na.rm=T)) keep[i] <- T
      }
      d <- cbind(as.data.frame(ws),
                 y=factor(ifelse(keep,"Y","N")))
      start <- as.formula(poly(colnames(ws)[edges],F))
      upper <- as.formula(poly(colnames(ws)[edges],T))
      fit <- step(glm(start,family=binomial(),data=d),
                  scope=list(lower=y~1,upper=upper),
                  k=log(nrow(d)))
      print(summary(fit))
      opar <- par(mfrow=c(2,1))
      ps <- predict(fit,type="link")
      d1 <- density(ps[keep],adjust=slider)
      d2 <- density(ps[!keep],adjust=slider)
      plot(d1,xlab="Probability",ylab="Density",main="",
           xlim=range(d1$x,d2$x),ylim=range(d1$y,d2$y),col=pal[1])
      lines(d2,col=pal[2])
      ps <- predict(fit,type="response")
      d1 <- density(ps[keep],adjust=slider)
      d2 <- density(ps[!keep],adjust=slider)
      plot(d1,xlab="Probability",ylab="Density",main="",
           xlim=range(d1$x,d2$x),ylim=range(d1$y,d2$y),col=pal[1])
      lines(d2,col=pal[2])
      par(opar)
    }
  }

  interactive.selection(action,nodes,cbind(edges$From,edges$To),
                        slider=list(initial=1,from=0,to=2),
                        perturb=T,monitor=T)
}



