#' Plot Entlastung Einkommensverteilung
#'
#' @param df dataframe Reform
#' @param breaks breaks X-Achse
#' @param limit limit X-Achse
#' @param width breite Gruppen
#' @param max_val Obergrenze Gruppe (symetrisch)
#' @param interactive Return interactive plot?
#'
#' @importFrom plotly ggplotly
#'
#' @returns ggplot or plotly
#' @export
#'
#' @examples
#' \dontrun{
#' gen_plot_EntlastungEinkommensverteilung(SPD2025_df)}
gen_plot_EntlastungEinkommensverteilung<-function(df,breaks=c(10000,30000,100000,500000),max_x=500000,width=1,max_val=8,interactive=F){

  # Subfunction creating groups for coloring####
  get_entlastung_group <- function(x,max_val, width=0.5, eps = 1e-12) {

    regular_breaks <- seq(-max_val, max_val, by = width)

    # 2) add a tiny break above zero
    all_breaks <- sort(unique(c(regular_breaks, 0, eps)))

    # 3) cut sequence
    x_cut <- cut(x, breaks = all_breaks, include.lowest = TRUE, right = FALSE)

    # 4) rename the [0, eps) bin to "0" and [eps,1) to (0,1)
    zero_level <- grep("0,", levels(x_cut), value = TRUE)  # i.e. "[0,1e-12)"
    if (length(zero_level) == 1) {
      levels(x_cut)[levels(x_cut) == zero_level] <- "0"
    }

    zero_level2 <- grep(paste0("\\[",as.character(eps),","), levels(x_cut), value = TRUE)  # i.e. "[1e-12,1)"
    if (length(zero_level2) == 1) {
      levels(x_cut)[levels(x_cut) == zero_level2] <- "(0,1)"
    }

    # 5) reorder factor levels from negative to zero to positive
    orig_lvls <- levels(x_cut)
    neg_lvls  <- orig_lvls[grepl("\\[-", orig_lvls) & orig_lvls != "0"]
    pos_lvls  <- setdiff(orig_lvls, c(neg_lvls, "0"))
    new_lvls  <- c(neg_lvls, "0", pos_lvls)

    # make it an ordered factor
    x_cut <- factor(x_cut, levels = new_lvls, ordered = TRUE)
    x_cut
  }

  ##prepare data####

  labels=formatC(breaks,big.mark = ".",decimal.mark = ",", format = "f", digits = 0)
  vis_data<-df%>%
    filter(ZvE>=10000)%>%
    mutate(Steuerpflichtige_norm_smooth=100*Steuerpflichtige_norm_smooth/sum(Steuerpflichtige_norm_smooth,na.rm = TRUE),
           Entlastung_group=get_entlastung_group(Entlastung_inklSoli_pcZvE,max_val=max_val,width=width))%>%
    arrange(ZvE)%>%
    filter(row_number() %% 400 == 1|lag(Entlastung_group)!=Entlastung_group|lag(Entlastung_group,200)!=Entlastung_group|lead(Entlastung_group)!=Entlastung_group|lead(Entlastung_group,200)!=Entlastung_group) #makes it faster lags necessary to avoid white lines


  #actual plot####
  p<-vis_data%>%
    ggplot(aes(log(ZvE),Steuerpflichtige_norm_smooth))

  for(i in seq(0,max_x,by=5000)){
    p<-p+
      geom_ribbon(
        data=subset(vis_data,ZvE>=i-400 &ZvE<=i+5000+400),
        aes(x=log(ZvE),ymin=0,ymax = Steuerpflichtige_norm_smooth,fill=Entlastung_group),
        position = "identity",alpha=1
      )
  }

  #add informative lines
  p<-p+
    geom_line(aes(text=paste0("Anteil Steuerpflichte: ",round(100*AnteilZvE,1),"%<br>",
                              "Zu versteuerndes Einkommen: ",round(ZvE),"€<br>",
                              "relative Entlastung: ", round(Entlastung_inklSoli_pcZvE,1),"%<br>",
                              "absolute Entlastung: ", round(Entlastung_inklSoli_absolut),"€")
    ),
    alpha=0
    )+
    geom_vline(xintercept = log(12085),linetype="solid",color="#181c44")+
    geom_vline(xintercept=log(vis_data[which.min(abs(vis_data$AnteilZvE-0.5)),]$ZvE),linetype="dotted",color="#181c44")+
    geom_text(x=log(vis_data[which.min(abs(vis_data$AnteilZvE-0.5)),]$ZvE)+0.3,y=0.003,label="Median",size=2.5,family="Open Sans",color="#181c44")+
    #geom_vline(xintercept=log(vis_data[which.min(abs(vis_data$AnteilZvE-0.9)),]$ZvE),linetype="dotted")+
    geom_vline(xintercept=log(vis_data[which.min(abs(vis_data$AnteilZvE-0.95)),]$ZvE),linetype="dotted",color="#181c44")+
    geom_text(x=log(vis_data[which.min(abs(vis_data$AnteilZvE-0.95)),]$ZvE)+0.4,y=0.003,label="95 Prozent",size=2.5,family="Open Sans",color="#181c44")



  levels_groups<-vis_data$Entlastung_inklSoli_pcZvE%>%get_entlastung_group(width=width,max_val=max_val)%>%levels()
  colors<-colorRampPalette(c("#ee6174","grey95","#57C773"))(length(levels_groups))
  p<-p+
    scale_x_continuous(limits=c(log(10000),log(500000)),breaks = log(breaks),labels = labels,expand=c(0.06,0.06))+
    scale_fill_manual(
      values=colors,
      breaks=levels_groups,
      drop=F)+
    scale_color_manual(
      values=colors,
      breaks=levels_groups,
      drop=F)+
    labs(x=NULL,y=NULL,title="Entlastung",subtitle="nach Verteilung des zu verst. Einkommens")+
    theme_dz(base_size = 10)+
    theme( axis.text.y  = element_blank(),
           panel.grid.major.y = element_blank(),
           legend.position = "none"
    )# axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))

  if(interactive){
    p<-ggplotly(p,tooltip="text")
    return(p)
  }else{
    return(p)
  }
}
