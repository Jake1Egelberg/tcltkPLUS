library(tcltk)
library(tkrplot)
library(dplyr)

#Define fonts
head_font<-tkfont.create(size=16,weight="bold")
subhead_font<-tkfont.create(size=12,weight="bold")
norm_font<-tkfont.create(size=10)

#Simple single entry box in a new window, saves input as tclval
entrybox_window<-function(title,label,width,tclvar){
  
  submit_entry<-function(){
    print(tclvalue(tclvar))
    .GlobalEnv$tclval<-tclvalue(tclvar)
    tkdestroy(top)
    print("Input saved in variable <tclval>")
  }
  
  .GlobalEnv$top<-tktoplevel()
  tkwm.title(top,title)
  tkwm.geometry(top,"+500+100")
  frame<-tkframe(top)
  tkgrid(frame,padx=30,pady=20)
  lbl<-tklabel(frame,text=label,font=subhead_font,justify="center")
  tkgrid(lbl,row=1,column=1)
  entry<-tkentry(frame,width=width,textvariable=tclvar,justify="center")
  tkgrid(entry,row=2,column=1,pady=5)
  tkbind(entry,"<Return>",submit_entry)
  submit<-tkbutton(frame,text="Submit",command=submit_entry)
  tkgrid(submit,row=3,column=1)
  tkwait.window(top)
  
}

#Simple single entry box in a new frame, saves input as tclval
entrybox_frame<-function(frame,label,width,tclvar){
  
  submit_entry<-function(){
    print(tclvalue(tclvar))
    .GlobalEnv$tclval<-tclvalue(tclvar)
    print("Input saved in variable <tclval>")
  }
  
  lbl<-tklabel(frame,text=label,font=subhead_font,justify="center")
  tkgrid(lbl,row=1,column=1)
  entry<-tkentry(frame,width=width,textvariable=tclvar,justify="center")
  tkgrid(entry,row=2,column=1,pady=5)
  tkbind(entry,"<Return>",submit_entry)
  submit<-tkbutton(frame,text="Submit",command=submit_entry)
  tkgrid(submit,row=3,column=1)
}

#Display a dataframe of information in a new window
table_window<-function(title,dataframe){

  .GlobalEnv$top<-tktoplevel()
  tkwm.title(top,title)
  tkwm.geometry(top,"+500+100")
  frame<-tkframe(top)
  tkgrid(frame,padx=30,pady=20)
  #Column titles
  lapply(1:ncol(dataframe),function(x){
    tmplabel<-names(dataframe)[x]
    assign(paste("lbl",x,sep=""),
           tklabel(frame,text=tmplabel,font=subhead_font),
           envir = .GlobalEnv)
    tkgrid(eval(parse(text=paste("lbl",x,sep=""))),row=1,column=x,padx=20)
  })
  #Row information
  lapply(1:nrow(dataframe),function(y){
    tmprow<-dataframe[y,]
    lapply(1:ncol(tmprow),function(z){
      tmprowdata<-tmprow[,z]
      assign(paste("row",y,"data",z,sep=""),
             tklabel(frame,text=tmprowdata,font=norm_font),
             envir=.GlobalEnv)
      tkgrid(eval(parse(text=paste("row",y,"data",z,sep=""))),row=y+1,column=z,padx=3)
    })
  })
  tkwait.window(top)
  
}

#Display a dataframe in a given frame
table_frame<-function(frame,dataframe){
 
  #Column titles
  lapply(1:ncol(dataframe),function(x){
    tmplabel<-names(dataframe)[x]
    assign(paste("lbl",x,sep=""),
           tklabel(frame,text=tmplabel,font=subhead_font),
           envir = .GlobalEnv)
    tkgrid(eval(parse(text=paste("lbl",x,sep=""))),row=1,column=x,padx=20)
  })
  #Row information
  lapply(1:nrow(dataframe),function(y){
    tmprow<-dataframe[y,]
    lapply(1:ncol(tmprow),function(z){
      tmprowdata<-tmprow[,z]
      assign(paste("row",y,"data",z,sep=""),
             tklabel(frame,text=tmprowdata,font=norm_font),
             envir=.GlobalEnv)
      tkgrid(eval(parse(text=paste("row",y,"data",z,sep=""))),row=y+1,column=z,padx=3)
    })
  })
  
}

#Display a plot in a given frame
plot_frame<-function(frame,plot,hscale,vscale){
  .GlobalEnv$tk_plot<-tkrplot(frame,fun=function(p)plot(plot),hscale=hscale,vscale=vscale)
  tkgrid(tk_plot)
}

#Scrollable multi-option list
#Accepted widget types:
  #Label, entry, combobox
#Accepts input of 3 lists with widget names, types, and data defined
  #widget_names<-list("one","two","three","four")
  #widget_types<-list("label","entry","entry","combobox")
  #widget_data<-list("hello","TEST","goodbye",c("yes","no","bye"))
customlist_frame<-function(frame,widget_names,widget_types,widget_data){
  
  #Create frame
  create_frame<-function(){
    .GlobalEnv$list_frame<-tkframe(frame)
    tkgrid(list_frame,row=1,column=1) 
  }
  create_frame()
  
  #Get vector of names and types
  widget_names_vec<-unlist(widget_names)
  widget_types_vec<-unlist(widget_types)
  
  #Organize as df
  widget_info<-data.frame(Widget_Name=widget_names_vec,
                          Widget_Type=widget_types_vec)
  widget_info$ID<-1:nrow(widget_info)
  
  #Define number of entires
  .GlobalEnv$entry_num<-c(1)
  
  #Reset frame
  reset_frame<-function(){
    tkgrid.remove(list_frame)
    create_frame()
  }
  
  #Add a new row of entries
  add_new_entries<-function(){
    .GlobalEnv$entry_num<-c(entry_num,max(entry_num)+1)
    print(entry_num)
    
    reset_frame()
    create_widgets()
  }
  
  #Scrape entries
  scrape_entries<-function(){
    
    .GlobalEnv$all_data<-list()
    lapply(entry_num,function(p){
      
      #Get variable names
      var_names<-paste(widget_info$Widget_Name,"_",p,sep="")
      
      row_data<-unlist(lapply(var_names,function(o){
        tryCatch(tclvalue(eval(parse(text=o))),error=function(e)NA)
      }))
      
      widget_inputs<-data.frame(Row=p,
                                Widget=widget_info$Widget_Name,
                                Input=row_data)
      all_data[[length(all_data)+1]]<-widget_inputs
      #print(all_data)
      .GlobalEnv$all_data<-all_data
    })
    
    .GlobalEnv$all_data_cur<-bind_rows(all_data)
    print(all_data_cur)
  }
  
  #Create widgets function
  create_widgets<-function(){
    
    #For each entry/row...
    lapply(entry_num,function(x){
      
      #Create each widget
      lapply(1:nrow(widget_info),function(y){
        
        #Get selected widget
        sel_widg<-widget_info[y,]
        
        #Get widget name
        widg_full_name<-paste("widget",sel_widg$Widget_Name,"row",x,"col",y,sep="")
        
        if(sel_widg$Widget_Type=="label"){
          #If widget is a label
          
          #Create widget
          assign(widg_full_name,
                 tklabel(list_frame,text=widget_data[[sel_widg$ID]],font=norm_font),
                 envir=.GlobalEnv)
          tkgrid(eval(parse(text=widg_full_name)),row=x,column=y,padx=20)
          
        } else if(sel_widg$Widget_Type=="entry"){
          #If widget is an entry
          
          var_name<-paste(sel_widg$Widget_Name,"_",x,sep="")
          
          #Assign widget name to textvariable
          tryCatch(tclvalue(eval(parse(text=var_name))),
                   error=function(e){
                     assign(var_name,
                            tclVar(widget_data[[sel_widg$ID]]),
                            envir=.GlobalEnv)
                   })
          
          #Create widget
          assign(widg_full_name,
                 tkentry(list_frame,width=15,justify="center",font=norm_font,textvariable=eval(parse(text=var_name))),
                 envir=.GlobalEnv)
          tkgrid(eval(parse(text=widg_full_name)),row=x,column=y,padx=20)
          
        } else if(sel_widg$Widget_Type=="combobox"){
          
          #Assign widget name to textvariable
          
          var_name<-paste(sel_widg$Widget_Name,"_",x,sep="")
          
          #Assign widget name to textvariable
          tryCatch(tclvalue(eval(parse(text=var_name))),
                   error=function(e){
                     assign(var_name,
                            tclVar(widget_data[[sel_widg$ID]][1]),
                            envir=.GlobalEnv)
                   })
          
          #Create widget
          assign(widg_full_name,
                 ttkcombobox(list_frame,width=15,justify="center",font=norm_font,values=widget_data[[sel_widg$ID]],textvariable=eval(parse(text=var_name))),
                 envir=.GlobalEnv)
          tkgrid(eval(parse(text=widg_full_name)),row=x,column=y,padx=20)
          
          
        }
        else{
          #Skip widget
          print(paste("Skipping ",sel_widg$Widget_Name,sep=""))
        }
        
        
      })
      
      if(x==1){
        #Add [+] function
        assign(paste("add",x,sep=""),
               tkbutton(list_frame,text="[+]",font=norm_font,command=add_new_entries),
               envir=.GlobalEnv)
        tkgrid(eval(parse(text=paste("add",x,sep=""))),row=x,column=(nrow(widget_info)+1))
      }
    
      #Remove function
      assign(paste("remove",x,sep=""),function(){
        
        .GlobalEnv$entry_num<-entry_num[-which(entry_num==x)]
        print(entry_num)
        
        #Reset variables
        lapply(1:nrow(widget_info),function(z){
          rem_widg<-widget_info[z,]
          varname<-paste(rem_widg$Widget_Name,"_",x,sep="")
          tryCatch(assign(varname,tclVar(widget_data[[rem_widg$ID]][1]),envir=.GlobalEnv),error=function(e)print("no var"))
        })
        
        reset_frame()
        create_widgets()
        
      },envir=.GlobalEnv)
      
      #Add [-] function
      assign(paste("sub",x,sep=""),
             tkbutton(list_frame,text="[-]",font=norm_font,command=eval(parse(text=paste("remove",x,sep="")))),
             envir=.GlobalEnv)
      tkgrid(eval(parse(text=paste("sub",x,sep=""))),row=x,column=(nrow(widget_info)+2),padx=10)
      if(x==1){
        tkconfigure(eval(parse(text=paste("sub",x,sep=""))),state="disabled")
      }
      
      
    }) 
    
    submit<-tkbutton(list_frame,text="Submit",command=scrape_entries)
    tkgrid(submit,row=max(entry_num)+1,column=1,columnspan=nrow(widget_info),pady=10)
  }
  
  create_widgets()
  
  #End scrollable list function
}

#Simple checklist window
  #Accepts vector of options
checklist_window<-function(title,options,prechecked){
  
  tryCatch(.GlobalEnv$precheck_status<-prechecked,
           error=function(e){
           .GlobalEnv$precheck_status<-FALSE  
           })
  
  get_checks<-function(){
    out<-lapply(1:length(options),function(z){
      tclvalue(eval(parse(text=paste("checkvar",z,sep=""))))
    })
    .GlobalEnv$out_cur<-unlist(out)
    tkdestroy(top)
    print(out_cur)
  }
  
  .GlobalEnv$top<-tktoplevel()
  tkwm.title(top,title)
  tkwm.geometry(top,"+500+100")
  frame<-tkframe(top)
  tkgrid(frame,padx=30,pady=20)
  
  lapply(1:length(options),function(x){
    
    #Create variable
    if(precheck_status==FALSE){
      assign(paste("checkvar",x,sep=""),
             tclVar(0),envir=.GlobalEnv)
    } else{
      assign(paste("checkvar",x,sep=""),
             tclVar(1),envir=.GlobalEnv) 
    }
    
    #Create widget
    assign(paste("check",x,sep=""),
           tkcheckbutton(frame,text=options[x],variable=eval(parse(text=paste("checkvar",x,sep="")))),
           envir=.GlobalEnv)
    tkgrid(eval(parse(text=paste("check",x,sep=""))),row=x,column=1,sticky="w")
    
  })
  
  submit<-tkbutton(frame,text="Submit",command=get_checks)
  tkgrid(submit,row=length(options)+1,column=1,pady=10)
  tkwait.window(top)
  
}

#Simple checklist frame
  #Accepts vector of options
checklist_frame<-function(frame,options,prechecked){
  
  tryCatch(.GlobalEnv$precheck_status<-prechecked,
           error=function(e){
             .GlobalEnv$precheck_status<-FALSE  
           })
  
  get_checks<-function(){
    out<-lapply(1:length(options),function(z){
      tclvalue(eval(parse(text=paste("checkvar",z,sep=""))))
    })
    .GlobalEnv$out_cur<-unlist(out)
    print(out_cur)
  }
  
  lapply(1:length(options),function(x){
    
    #Create variable
    if(precheck_status==FALSE){
      assign(paste("checkvar",x,sep=""),
             tclVar(0),envir=.GlobalEnv)
    } else{
      assign(paste("checkvar",x,sep=""),
             tclVar(1),envir=.GlobalEnv) 
    }
    
    #Create widget
    assign(paste("check",x,sep=""),
           tkcheckbutton(frame,text=options[x],variable=eval(parse(text=paste("checkvar",x,sep="")))),
           envir=.GlobalEnv)
    tkgrid(eval(parse(text=paste("check",x,sep=""))),row=x,column=1,sticky="w")
    
  })
  
  submit<-tkbutton(frame,text="Submit",command=get_checks)
  tkgrid(submit,row=length(options)+1,column=1,pady=10)
  tkwait.window(top)
}




