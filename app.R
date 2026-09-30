library(shiny)
source("R/core.R",local=TRUE)
all_data <- load_quality_data()
scenario_names <- c("機台異常"="machine","材料異常（機台易被誤判）"="material","材料 × 機台交互作用"="interaction")
models <- lapply(c("machine","material","interaction"),function(s) fit_quality_model(all_data[all_data$scenario==s,]))
names(models)<-c("machine","material","interaction")
pct<-function(x)ifelse(is.na(x),"—",sprintf("%.1f%%",100*x))
pfmt<-function(x)format.pval(x,digits=3,eps=.0001)
heat_color<-function(rate,n) {
  if(n<30||is.na(rate))return("#edf0f2")
  t<-min(1,max(0,rate/.5));rgb((255-47*t)/255,(248-135*t)/255,(236-185*t)/255)
}
ui<-fluidPage(
  tags$head(tags$style(HTML("
    body{font-family:system-ui,sans-serif;color:#243642;background:#f4f7f8;} .container-fluid{max-width:1200px;margin:auto;padding:22px;}
    h1{font-size:28px;}h3{font-size:19px;} .panelbox{background:#fff;padding:20px;border-radius:10px;margin:16px 0;}
    .heat{width:100%;table-layout:fixed;border-collapse:separate;border-spacing:6px;}.heat th{text-align:center;font-weight:500;padding:6px;}
    .heat button{width:100%;min-height:58px;border:2px solid transparent;border-radius:5px;color:#243642;font-size:17px;}
    .heat button.selected{border-color:#243642;}.heat small{display:block;font-size:12px;}.subtle{color:#536777;} .metric{font-size:19px;}
    @media(max-width:600px){.container-fluid{padding:10px}.panelbox{padding:12px}.heat button{font-size:15px;}}
  "))),
  h1("製程品質實驗室"),p(class="subtle","MES × 設備資料 × QMS｜全部為合成資料；用觀察、預測和試驗計畫學習製造決策。"),
  selectInput("scenario","模擬情境",scenario_names,selected="material"),
  tabsetPanel(
    tabPanel("1 發現與追查",fluidRow(
      column(6,selectInput("product","觀察產品",c("全部"="all","Model-A","Model-B"))),
      column(6,selectInput("period","生產日期",c("9 月 1–30 日"="all","1–20 日"="early","21–30 日"="late")))),
      p(textOutput("cohort_summary")),
      fluidRow(column(6,div(class="panelbox",h3("五道製程：哪台設備偏高？"),uiOutput("heat"))),
               column(6,div(class="panelbox",h3(textOutput("matrix_title",inline=TRUE)),uiOutput("matrix")))),
      p(class="subtle","格子顯示最終尺寸不良率與樣本數。固定色階 0–50%（以上飽和）；少於 30 件為灰色。"),
      div(class="panelbox",h3("點選比較"),uiOutput("comparison"),uiOutput("interpretation")),
      tags$details(tags$summary("揭示模擬機制"),textOutput("truth")),
      p(class="subtle","同一件產品會經過五道製程，不能加總各格當作產量。調整材料比例只是觀察資料的比較，不是根因證明。")),
    tabPanel("2 預測與改善",p("評估第三道製程。此頁使用完整情境的前 20 天模型，不隨分析頁篩選重新訓練。"),
      fluidRow(column(3,selectInput("machine","第三道製程設備",c("A","B","C"),selected="B")),
        column(3,selectInput("batch","材料批次",c("M01","M02","M03"),selected="M03")),
        column(3,selectInput("plan_product","方案產品",c("Model-A","Model-B"))),
        column(3,selectInput("shift","班別",c("白班"="Day","夜班"="Night")))),
      fluidRow(column(6,sliderInput("temp","製程溫度（示意 °C）",60,100,value=90,step=5)),
               column(6,sliderInput("speed","加工速度（額定百分比）",80,120,value=110,step=5))),
      selectInput("limit","可接受的模型預估不良率上限",c("5%"=.05,"10%"=.1,"20%"=.2,"30%"=.3),selected=.1),
      div(class="panelbox",uiOutput("current"),h3("方案比較"),tableOutput("plans"),textOutput("feasibility")),
      p(class="subtle","限制：每小時合格品至少 160 件；搜尋僅改溫度與速度，保持目前機台、材料、型號和班別。比較中的換料／換機列是另外的候選方案。"),
      p(class="subtle","示意成本：設備 NT$1,200／小時、材料 NT$45／投入件、報廢處理 NT$12／不良件；額定 200 件／小時。不含切換、停機與不同材料差價。"),
      p("模型預測不等於實際改善效果。用相同材料與生產條件做對照試產，驗證改善幅度後再擴大。")),
    tabPanel("3 資料與驗證",div(class="panelbox",h3("三份資料，以產品 ID 串接"),
      tableOutput("sources"),h3("合併後資料範例"),tableOutput("sample"),downloadButton("download","下載目前分析篩選資料")),
      div(class="panelbox",h3("時間切分的模型驗證"),tableOutput("metrics"),h3("測試集校準：五組風險"),tableOutput("calibration")),
      p(class="subtle","邏輯迴歸含機台×材料交互作用、溫度及其平方、速度、產品與班別。4000 件訓練、2000 件測試。模型不使用生成機率或最終檢驗後資訊作為輸入。"),
      p(class="subtle","Brier score 衡量機率誤差，越低越好；基準永遠預測訓練集不良率。AUC 衡量風險排序。合成資料中的表現不代表真實工廠效果。"))
  )
)
server<-function(input,output,session) {
  selected<-reactiveVal(list(stage=3L,machine="B"))
  d<-reactive({req(input$scenario,input$product,input$period);cohort_data(all_data,input$scenario,input$product,input$period)})
  stats<-reactive(heatmap_summary(d()))
  chosen<-reactive({s<-selected();x<-stats();x[x$stage==s$stage&x$machine==s$machine,]})
  observeEvent(input$cell,{if(input$cell$stage %in% 1:5 && input$cell$machine %in% c("A","B","C")) selected(list(stage=as.integer(input$cell$stage),machine=input$cell$machine))})
  output$cohort_summary<-renderText(sprintf("%s 件產品 · %s 件尺寸不良 · 整體不良率 %s",format(nrow(d()),big.mark=","),sum(d()$defect),pct(mean(d()$defect))))
  output$heat<-renderUI({
    x<-stats();s<-selected()
    tags$table(class="heat",tags$thead(tags$tr(tags$th("製程／機台"),lapply(c("A","B","C"),tags$th))),
      tags$tbody(lapply(1:5,function(i)tags$tr(tags$th(paste0("製程 ",i)),lapply(c("A","B","C"),function(m){
        z<-x[x$stage==i&x$machine==m,]
        tags$td(tags$button(type="button",class=if(s$stage==i&&s$machine==m)"selected" else "",
          `aria-label`=sprintf("製程 %d %s 機，不良率 %s，樣本 %d",i,m,pct(z$rate),z$n),
          style=paste0("background:",heat_color(z$rate,z$n)),onclick=sprintf("Shiny.setInputValue('cell',{stage:%d,machine:'%s'},{priority:'event'});",i,m),pct(z$rate),tags$small(paste0("n=",z$n))))
      })))))
  })
  output$matrix_title<-renderText(sprintf("製程 %d：材料 × 機台",selected()$stage))
  output$matrix<-renderUI({
    x<-material_matrix(d(),selected()$stage)
    tags$table(class="heat",tags$thead(tags$tr(tags$th("批次／機台"),lapply(c("A","B","C"),tags$th))),
      tags$tbody(lapply(c("M01","M02","M03"),function(b)tags$tr(tags$th(b),lapply(c("A","B","C"),function(m){
        z<-x[x$batch==b&x$machine==m,];tags$td(style=paste0("text-align:center;padding:12px;background:",heat_color(z$rate,z$n)),pct(z$rate),tags$small(sprintf("%d／%d",z$bad,z$n)))
      })))))
  })
  output$comparison<-renderUI({z<-chosen();tagList(
    p(class="metric",sprintf("製程 %d／%s 機：%s（%d／%d） vs 同製程其他機台 %s（%d／%d）",z$stage,z$machine,pct(z$rate),z$bad,z$n,pct(z$other_rate),z$other_bad,z$other_n)),
    p(sprintf("若兩組都有相同材料比例：%s vs %s；差距 %+.1f 個百分點。",pct(z$std),pct(z$std_other),100*(z$std-z$std_other))),
    p(class="subtle",sprintf("原始 2×2 比較：Fisher p %s；15 次比較 BH 調整後 p %s。",pfmt(z$p),pfmt(z$q))))})
  output$interpretation<-renderUI({z<-chosen();raw<-z$rate-z$other_rate;adj<-z$std-z$std_other
    p(if(abs(raw)>.05&&abs(adj)<abs(raw)*.35)"控制材料組成後差距大幅縮小：優先追查材料分配與批次品質。" else
      if(adj>.05)"相同材料比例下仍有差距：再看是否所有材料都偏高，或只有特定材料×機台組合。" else
        "此機台沒有明顯的調整後偏高訊號；仍需查看分層樣本與現場條件。")})
  output$truth<-renderText(switch(input$scenario,
    machine="生成規則：製程 3／B 有額外風險，材料分配與機台獨立。",
    material="生成規則：M03 有額外風險；B 機有 75% 使用 M03，A／C 各約 12%。機台本身沒有額外效應。",
    interaction="生成規則：只有製程 3／B 與 M03 同時出現，才有額外風險。材料分配與機台獨立。"))
  model<-reactive(models[[input$scenario]])
  comparisons<-reactive(compare_plans(model(),input$machine,input$batch,input$temp,input$speed,input$plan_product,input$shift,as.numeric(input$limit)))
  output$current<-renderUI({z<-comparisons()$plans[1,];p(class="metric",sprintf("預估不良率 %s · 合格品 %.1f 件／小時 · 每合格件成本 NT$%.2f",pct(z$risk),z$good,z$cost))})
  output$plans<-renderTable({x<-comparisons()$plans;data.frame("方案"=x$plan,"設定"=sprintf("%s / %s / %.0f°C / %.0f%%",x$machine,x$batch,x$temp,x$speed),"不良率"=pct(x$risk),"合格件/時"=round(x$good,1),"NT$/合格件"=round(x$cost,2),"符合限制"=ifelse(x$feasible,"是","否"),check.names=FALSE)},striped=TRUE)
  output$feasibility<-renderText({x<-comparisons();if(is.null(x$best))"81 組同機台／材料參數中沒有可行方案；請評估換料／換機，或重新檢視限制。" else sprintf("81 組參數中，%d 組符合限制；表中顯示其中預估單件成本最低的一組。",x$eligible)})
  output$sources<-renderTable(data.frame("來源"=c("MES","設備資料","QMS"),"每情境列數"=c(6000,30000,6000),"主鍵"=c("unit_id","unit_id + stage","unit_id"),"內容"=c("型號、班別、材料、生產時間","設備、製程溫度、速度","最終尺寸檢驗結果"),check.names=FALSE))
  output$sample<-renderTable(head(d()[c("unit_id","material_batch","p3","temperature_c","speed_pct","defect")],6),striped=TRUE)
  output$metrics<-renderTable({m<-model();data.frame("指標"=c("訓練/測試件數","測試 AUC","測試 Brier","基準 Brier"),"值"=c(sprintf("%d / %d",m$train_n,m$test_n),sprintf("%.3f",m$auc),sprintf("%.4f",m$brier),sprintf("%.4f",m$baseline_brier)),check.names=FALSE)})
  output$calibration<-renderTable({x<-model()$calibration;data.frame("風險組"=1:5,"件數"=x$n,"平均預測"=pct(x$predicted),"觀測不良率"=pct(x$observed),check.names=FALSE)})
  output$download<-downloadHandler(function()"quality_cohort.csv",function(file)write.csv(d(),file,row.names=FALSE,fileEncoding="UTF-8"))
}
shinyApp(ui,server)
