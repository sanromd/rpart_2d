corners <- rbind(c(x=-Inf, y=-Inf), 
                 c(x=-Inf, y=Inf), 
                 c(x=Inf, y=-Inf), 
                 c(x=Inf, y=Inf))

splits <- function(model) { 
  cbind(data.frame("variable"=row.names(model$split)), data.frame(model$split)) 
}

verticals <- function(split, varname) {
  unique(sort(split[which(split$variable==varname),]$index))
}

horizontals <- function(split, varname) {
  unique(sort(split[which(split$variable==varname),]$index, decreasing=TRUE))
}

boundary_points <- function(vertical, horizontal, corners) {
  x <- rbind(expand.grid(x=vertical, y=horizontal),
             corners,
             expand.grid(x=vertical, y=corners[,1]),
             expand.grid(x=corners[,2], y=horizontal))
  return(unique(x))
}

set_log_scales <- function(p1,logx,logy){
  if (logx){
    p1 <- p1 + scale_x_log10()
  }
  
  if (logy){
    p1 <- p1 + scale_y_log10()
  }
  return(p1)  
}

plot_2d_scatter <- function(df, vx, vy, vq,
                            pcolor, pshape="none", pgroup="none",
                            xintercept, yintercept,
                            xlabel = "x", ylabel="y", clabel="color", 
                            dodge_width = 0.2, logx = FALSE, logy = FALSE){
  if(pshape != "none"){
    df[,pshape] = factor(df[,pshape])
    df <- as.data.frame(df)
  }
  
  if(pgroup != "none"){
    print("grouping")
    df <- df %>% group_by_(.dots = pgroup)
    df <- as.data.frame(df)
  }
  
  p1 <- ggplot(df, aes_string(x = vx, y = vy, color = pcolor, 
                              size = vq, shape = pshape), 
               position = "dodge") +
    geom_point(position = position_dodge(width = dodge_width)) +
    scale_color_viridis() + 
    #scale_color_manual(values=wes_palette(n=4, name="GrandBudapest"), name = clabel) +
    xlab(xlabel) + ylab(ylabel)
  p1 <- p1 +
    geom_vline(xintercept = vintersept, color = 'gray') + 
    geom_hline(yintercept = hintersept, color = 'gray')
  
  if (logx | logy){
    p1 <- set_log_scales(p1,logx,logy)
  }
  
  return(p1)
}

plot_2d_tree <- function(df, tree, vx, vy, vq, vpoint='q_max',
                         pcolor, pshape="none", pgroup="none",
                         xlabel = "x", ylabel="y", clabel="color", 
                         txt_size = 4, aspect = 3/5, 
                         dodge_width = 0.2, logx = FALSE, logy = FALSE,
                         scatter_plot = TRUE){
  out = tree
  
  # first get the horizontal and vertical intersects from the tree
  split <- splits(tree)
  vintersept <- sort(verticals(split, vx)) 
  hintersept <- sort(horizontals(split, vy))
  
  # add the minimum and maximum points
  vintersept = sort(unique(c(vintersept, min(df[,vx]), max(df[,vx]))))
  hintersept = sort(unique(c(hintersept, min(df[,vy]), max(df[,vy]))))
  pintersept = expand.grid(vintersept,hintersept)
  
  if (scatter_plot){
    p1_wbound_lines <- plot_2d_scatter(df, vx, vy, vq,
                                       pcolor, pshape, pgroup,
                                       xintercept, yintercept,
                                       xlabel = xlabel, ylabel=ylabel, clabel=clabel, 
                                       dodge_width = dodge_width, logx = logx, logy = logy)
    out$p_wbound_lines <- p1_wbound_lines
  }
 
  # generate the coordinates of the squares and the values of q there
  # there must be an easier and elgant way to do this... (later!)
  qs = NULL
  df[, "v1"] = df[, vx]
  df[, "v2"] = df[, vy]
  df[, 'qnorm'] = df[, vq]
  
  vpo <- vintersept[1]
  for (vp in tail(vintersept,-1)){
    hpo <- hintersept[1]
    for (hp in tail(hintersept,-1)){
      temp_qs <- df %>% 
        filter(v1<=vp,v1>=vpo,
               v2<=hp,v2>=hpo) %>%
        summarise(q_mean = mean(qnorm, na.rm = TRUE),
                  q_median = median(qnorm, na.rm=TRUE),
                  q_max = max(qnorm, na.rm=TRUE),
                  q_min = min(qnorm, na.rm=TRUE))
      xc <- (vp + vpo)/2
      yc <- (hp + hpo)/2
      yb <- hpo + 0.3*(hp-hpo)/2
      temp_qs <- round(temp_qs,3)
      qs <- rbind(qs,cbind(xc,yc,yb,vpo,vp,hpo,hp,temp_qs))
      hpo <- hp
    }
    vpo <- vp
  }
  
  out$qs = qs
  
  p1 <- ggplot(qs, aes(xmin = vpo, xmax = vp, ymin = hpo, ymax = hp)) + 
    geom_rect(aes(fill = q_mean), colour = "gray", alpha = 0.5) + 
    geom_point(aes_string(x = "xc", y = "yc", size = vpoint)) +
    scale_fill_viridis(discrete = FALSE) +
    geom_text(aes(x = xc, y = yb, 
                  label = ifelse(is.infinite(q_max), "", sprintf("%1.1f",q_max))), 
              fontface = "bold", family = "Arial", size = txt_size) +
    xlab(xlabel) + ylab(ylabel)
 
  if (logx | logy){
    p1 <- set_log_scales(p1,logx,logy)
  }
  
  out$p_tree2d = p1
  
  return(out)
}