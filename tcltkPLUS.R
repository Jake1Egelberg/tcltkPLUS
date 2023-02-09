library(tcltk)
library(tkrplot)
library(dplyr)

#Universally saves data to global variable user_input

#Define fonts
head_font<-tkfont.create(size=16,weight="bold")
subhead_font<-tkfont.create(size=12,weight="bold")
norm_font<-tkfont.create(size=10)

#Simplify user inputs
simplify_input<-function(input){
  .GlobalEnv$new_input<-str_replace_all(tolower(input),"[:punct:]|\\s","")
}

#Simple single entry box in a new window, saves input as user_input
entrybox_window<-function(title,width,preset){
  
  tryCatch(.GlobalEnv$tclvar<-tclVar(preset),
           error=function(e){
             .GlobalEnv$tclvar<-tclVar("")
           })
  
  submit_entry<-function(){
    .GlobalEnv$user_input<-tclvalue(tclvar)
    print(user_input)
    tkdestroy(top)
  }
  
  .GlobalEnv$top<-tktoplevel()
  tkwm.title(top,title)
  tkwm.geometry(top,"+500+100")
  frame<-tkframe(top)
  tkgrid(frame,padx=30,pady=20)
  
  
  header<-tklabel(frame,text=title,font=subhead_font)
  tkgrid(header,row=0,column=1,columnspan=2,pady=10)

  entry<-tkentry(frame,width=width,textvariable=tclvar,justify="center")
  tkgrid(entry,row=2,column=1,pady=5,columnspan=2)
  tkbind(entry,"<Return>",submit_entry)
  
  submit<-tkbutton(frame,text="Submit",command=submit_entry)
  tkgrid(submit,row=3,column=1,pady=10,columnspan=2)
  tkwait.window(top)
  
}

#Simple single entry box in a new frame, saves input as user_input
entrybox_frame<-function(frame,label,width,preset){
  
  tryCatch(.GlobalEnv$tclvar<-tclVar(preset),
           error=function(e){
             .GlobalEnv$tclvar<-tclVar("")
           })
  
  submit_entry<-function(){
    print(tclvalue(tclvar))
    .GlobalEnv$user_input<-tclvalue(tclvar)
    print("Input saved in variable <user_input>")
  }
  
  lbl<-tklabel(frame,text=label,font=subhead_font,justify="center")
  tkgrid(lbl,row=1,column=1)
  entry<-tkentry(frame,width=width,textvariable=tclvar,justify="center")
  tkgrid(entry,row=2,column=1,pady=5)
  tkbind(entry,"<Return>",submit_entry)
  submit<-tkbutton(frame,text="Submit",command=submit_entry)
  tkgrid(submit,row=3,column=1)
}

#Multientry box window
  #Saves data to user_input
multientrybox_window<-function(title,entry_width,label_vec){
  
  .GlobalEnv$top<-tktoplevel()
  tkwm.title(top,title)
  tkwm.geometry(top,"+500+100")
  frame<-tkframe(top)
  tkgrid(frame,padx=30,pady=20)
  
  header<-tklabel(frame,text=title,font=subhead_font)
  tkgrid(header,row=0,column=1,columnspan=2,pady=10)
  
  submit_entries<-function(){
    
    .GlobalEnv$inputs<-lapply(1:length(label_vec),function(y){
      tmp<-simplify_input(label_vec[y])
      tryCatch(.GlobalEnv$out<-tclvalue(eval(parse(text=paste(tmp,"entryvar",y,sep="")))),
               error=function(e){.GlobalEnv$out<-NA})
      
    })
    .GlobalEnv$user_input<-unlist(inputs)
    tkdestroy(top)
    print(user_input)
  }
  
  #Create widgets
  lapply(1:length(label_vec),function(x){
    og<-label_vec[x]
    tmp<-simplify_input(label_vec[x])
    
    #Create label
    assign(paste(tmp,"label",x,sep=""),
           tklabel(frame,text=og,font=norm_font),envir=.GlobalEnv)
    tkgrid(eval(parse(text=(paste(tmp,"label",x,sep="")))),row=x,column=1,pady=5,sticky="e")
    
    #Create entry var
    assign(paste(tmp,"entryvar",x,sep=""),
           tclVar(""),envir=.GlobalEnv)
    
    #Create entry
    assign(paste(tmp,"entry",x,sep=""),
           tkentry(frame,width=entry_width,textvariable=eval(parse(text=paste(tmp,"entryvar",x,sep=""))),justify="center"),envir=.GlobalEnv)
    tkgrid(eval(parse(text=(paste(tmp,"entry",x,sep="")))),row=x,column=2,pady=5,sticky="e",padx=5)
    tkbind(eval(parse(text=paste(tmp,"entry",x,sep=""))),"<Return>",submit_entries)
  })
  
  submit<-tkbutton(frame,text="Submit",command=submit_entries)
  tkgrid(submit,row=length(label_vec)+1,column=1,pady=10,columnspan=2)
  tkwait.window(top)
  
}

#Multientry box frame
  #Saves data to user_input
multientrybox_frame<-function(frame,entry_width,label_vec){

  submit_entries<-function(){
    
    .GlobalEnv$inputs<-lapply(1:length(label_vec),function(y){
      tmp<-gsub(" ","",label_vec[y])
      tryCatch(.GlobalEnv$out<-tclvalue(eval(parse(text=paste(tmp,"entryvar",y,sep="")))),
               error=function(e){.GlobalEnv$out<-NA})
      
    })
    .GlobalEnv$user_input<-unlist(inputs)
    print(user_input)
  }
  
  #Create widgets
  lapply(1:length(label_vec),function(x){
    og<-label_vec[x]
    tmp<-gsub(" ","",label_vec[x])
    
    #Create label
    assign(paste(tmp,"label",x,sep=""),
           tklabel(frame,text=og,font=norm_font),envir=.GlobalEnv)
    tkgrid(eval(parse(text=(paste(tmp,"label",x,sep="")))),row=x,column=1,pady=5,sticky="e")
    
    #Create entry var
    assign(paste(tmp,"entryvar",x,sep=""),
           tclVar(""),envir=.GlobalEnv)
    
    #Create entry
    assign(paste(tmp,"entry",x,sep=""),
           tkentry(frame,width=entry_width,textvariable=eval(parse(text=paste(tmp,"entryvar",x,sep=""))),justify="center"),envir=.GlobalEnv)
    tkgrid(eval(parse(text=(paste(tmp,"entry",x,sep="")))),row=x,column=2,pady=5,sticky="e",padx=5)
    tkbind(eval(parse(text=paste(tmp,"entry",x,sep=""))),"<Return>",submit_entries)
  })
  
  submit<-tkbutton(frame,text="Submit",command=submit_entries)
  tkgrid(submit,row=length(label_vec)+1,column=1,pady=10,columnspan=2)
  
}

#Display a dataframe of information in a new window
table_window<-function(title,dataframe){
  
  .GlobalEnv$top<-tktoplevel()
  tkwm.title(top,title)
  tkwm.geometry(top,"+500+100")
  frame<-tkframe(top)
  tkgrid(frame,padx=30,pady=20)
  ttl_lbl<-tklabel(frame,text=title,font=head_font)
  tkgrid(ttl_lbl,row=0,column=1,columnspan=ncol(dataframe),pady=10)
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
  
  tryCatch(.GlobalEnv$hscale_val<-hscale,
           error=function(e){
             .GlobalEnv$hscale_val<-1
           })
  tryCatch(.GlobalEnv$vscale_val<-vscale,
           error=function(e){
             .GlobalEnv$vscale_val<-1
           })
  
  .GlobalEnv$tk_plot<-tkrplot(frame,fun=function(p)plot(plot),hscale=hscale_val,vscale=vscale_val)
  tkgrid(tk_plot)
}

#Scrollable multi-option list
  #Saves data to user_input, allows custom submit function
#Accepted widget types:
#Label, entry, combobox
#Accepts input of 3 lists with widget names, types, and data defined. Header is optional
  #widget_names<-list("one","two","three","four")
  #widget_types<-list("label","entry","combobox","combobox")
  #widget_data<-list(c("Hi","Bye"),c("TEST1","2"),c("yay","bad"),c("yes","WOOHGOO","bye"))
  #header<-list("","Entry1","Entry2","Combobox1")
#Can preset combobox variables with a list, each vector corresponds to new entry
  #combopresel<-list(c(1,2),
  #                  c(2,3),
  #                  c(2,1))
  
#top<-tktoplevel()
#frm<-tkframe(top)
#tkgrid(frm)
customlist_frame<-function(frame,widget_names,widget_types,widget_data,submit_fun,number_labels,entries,header,width,overwrite,combopresel){

  #Check if want to count labels
  tryCatch(.GlobalEnv$number_labels_status<-number_labels,
           error=function(e){.GlobalEnv$number_labels_status<-FALSE})
  
  #Check for header values
  tryCatch(.GlobalEnv$header_val<-unlist(header),
           error=function(e){.GlobalEnv$header_val<-NA})
  
  #Get width values
  tryCatch(.GlobalEnv$width_val<-width,
           error=function(e){.GlobalEnv$width_val<-15})
  
  #Get overwrite value
  tryCatch(.GlobalEnv$overwrite_val<-overwrite,
           error=function(e){.GlobalEnv$overwrite_val<-FALSE})
  
  #Get combobox presel
  tryCatch(.GlobalEnv$combopresel_val<-combopresel,
           error=function(e){.GlobalEnv$combopresel_val<-NA})
  
  #Get vector of names and types
  widget_names_vec<-unlist(widget_names)
  widget_types_vec<-unlist(widget_types)
  
  #Check that all same length
  header_check<-(length(which(is.na(header_val)==TRUE))>0)||(length(which(is.na(header_val)==TRUE))==0&&length(header_val)==length(widget_data))
  if(length(widget_names_vec)==length(widget_types_vec)&&length(widget_names_vec)==length(widget_data)&&header_check==TRUE){
    
    #Organize as df
    widget_info<-data.frame(Widget_Name=widget_names_vec,
                            Widget_Type=widget_types_vec)
    widget_info$ID<-1:nrow(widget_info)
    
    #Define number of entires
    tryCatch(.GlobalEnv$entry_num<-entries,
             error=function(e){
               .GlobalEnv$entry_num<-c(1)
             })
    
    #Create frame
    create_frame<-function(){
      .GlobalEnv$list_frame<-tkframe(frame)
      tkgrid(list_frame,row=1,column=1) 
    }
    create_frame()
    
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
      
      .GlobalEnv$user_input<-bind_rows(all_data)
      print(user_input)
      
      tryCatch(submit_fun(),error=function(e)print("Error in submit function"))
      
    }
    
    #Reset vars
    reset_vars<-function(variable_name){
      if(overwrite_val==TRUE){
        #Remove existing vars from global env
        assign(variable_name,NULL,envir=.GlobalEnv)
      }
    }
    
    #Create widgets function
    create_widgets<-function(){
      
      #For each entry/row...
      lapply(entry_num,function(x){
        
        #Count number of comboboxes in each row
        .GlobalEnv$combobox_count<-0
        
        #Create each widget
        lapply(1:nrow(widget_info),function(y){
          
          #Get selected widget
          sel_widg<-widget_info[y,]
          
          #Get widget name
          widg_full_name<-paste("widget",sel_widg$Widget_Name,"row",x,"col",y,sep="")
          
          if(sel_widg$Widget_Type=="label"){
            #If widget is a label
            
            if(number_labels_status==TRUE){
              label<-paste(widget_data[[sel_widg$ID]]," ",which(entry_num==x),sep="")
            } else{
              label<-widget_data[[sel_widg$ID]]
            }
            
            #Create widget
            assign(widg_full_name,
                   tklabel(list_frame,text=label,font=norm_font),
                   envir=.GlobalEnv)
            tkgrid(eval(parse(text=widg_full_name)),row=x,column=y,padx=10)
            
          } else if(sel_widg$Widget_Type=="entry"){
            #If widget is an entry
            
            var_name<-paste(sel_widg$Widget_Name,"_",x,sep="")
        
            #If on startup (first time running)
            reset_vars(var_name)
            
            #Assign widget name to textvariable
            tryCatch(tclvalue(eval(parse(text=var_name))),
                     error=function(e){
                       assign(var_name,
                              tclVar(widget_data[[sel_widg$ID]][which(entry_num==x)]),
                              envir=.GlobalEnv)
                     })
            
            #Create widget
            assign(widg_full_name,
                   tkentry(list_frame,width=width_val,justify="center",font=norm_font,textvariable=eval(parse(text=var_name))),
                   envir=.GlobalEnv)
            tkgrid(eval(parse(text=widg_full_name)),row=x,column=y,padx=20)
            tkbind(eval(parse(text=widg_full_name)),"<Return>",scrape_entries)
            
          } else if(sel_widg$Widget_Type=="combobox"){
            
            #Yay another combobox!
            .GlobalEnv$combobox_count<-combobox_count+1
            
            #Assign widget name to textvariable
            var_name<-paste(sel_widg$Widget_Name,"_",x,sep="")
            print(var_name)
            #If on startup (first time running)
            reset_vars(var_name)
            
            #Assign widget name to textvariable
            tryCatch(tclvalue(eval(parse(text=var_name))),
                     error=function(e){
                       
                       if(length(which(is.na(combopresel_val)==TRUE))==0){
                         comboval<-widget_data[[sel_widg$ID]][combopresel_val[[which(entry_num==x)]][combobox_count]]
                       } else{
                         comboval<-""
                       }
                       
                       assign(var_name,
                              tclVar(comboval),
                              envir=.GlobalEnv)
                     })
            
            #Create widget
            assign(widg_full_name,
                   ttkcombobox(list_frame,width=width_val,justify="center",font=norm_font,values=widget_data[[sel_widg$ID]],textvariable=eval(parse(text=var_name))),
                   envir=.GlobalEnv)
            tkgrid(eval(parse(text=widg_full_name)),row=x,column=y,padx=10)
            tkbind(eval(parse(text=widg_full_name)),"<Return>",scrape_entries)
            
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
          tkgrid(eval(parse(text=paste("add",x,sep=""))),row=x,column=(nrow(widget_info)+1),padx=10)
        }
        
        #Remove function
        assign(paste("remove",x,sep=""),function(){
          
          .GlobalEnv$entry_num<-entry_num[-which(entry_num==x)]
          print(entry_num)
          
          #Reset variables
          lapply(1:nrow(widget_info),function(z){
            rem_widg<-widget_info[z,]
            varname<-paste(rem_widg$Widget_Name,"_",x,sep="")
            tryCatch(assign(varname,tclVar(""),envir=.GlobalEnv),error=function(e)print("no var"))
          })
          
          reset_frame()
          create_widgets()
          
        },envir=.GlobalEnv)
        
        #Add [-] function
        assign(paste("sub",x,sep=""),
               tkbutton(list_frame,text="[-]",font=norm_font,command=eval(parse(text=paste("remove",x,sep="")))),
               envir=.GlobalEnv)
        tkgrid(eval(parse(text=paste("sub",x,sep=""))),row=x,column=(nrow(widget_info)+2))
        if(x==1){
          tkconfigure(eval(parse(text=paste("sub",x,sep=""))),state="disabled")
        }
        
        
      }) 
      
      submit<-tkbutton(list_frame,text="Submit",command=scrape_entries)
      tkgrid(submit,row=max(entry_num)+1,column=1,columnspan=(nrow(widget_info)+10),pady=10)
      
      #Add header if header
      if(length(which(is.na(header_val)==FALSE)==0)){
        
        lapply(1:length(header_val),function(t){
          tmphead<-simplify_input(header_val[t])
          assign(paste(tmphead,"HEAD",sep=""),
                 tklabel(list_frame,text=header_val[t],justify="center",font=subhead_font),
                 envir=.GlobalEnv)
          tkgrid(eval(parse(text=paste(tmphead,"HEAD",sep=""))),row=0,column=t)
        })
        
      }
      
    }
    
    create_widgets()

  } else{
    print("ERROR: Your inputs are not of the same length!")
  }
  
  #End scrollable list function
}
#customlist_frame(frm,widget_names,widget_types,widget_data,combopresel = combopresel_raw,entries=c(1:9),overwrite=TRUE)

#Simple checklist window
  #Saves data as user_input
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
    .GlobalEnv$user_input<-unlist(out)
    tkdestroy(top)
    print(user_input)
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
  #Saves data as user_input
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
    .GlobalEnv$user_input<-unlist(out)
    print(user_input)
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
}

#Simple combobox frame
combobox_frame<-function(frame,label,options,width,sel_fun,presel){

  if(presel==0){
    .GlobalEnv$combovar<-tclVar("")
  } else{
    .GlobalEnv$combovar<-tclVar(options[presel])
  }

  get_selection<-function(){
    .GlobalEnv$user_input<-tclvalue(combovar)
    print(user_input)
    tryCatch(sel_fun(),error=function(e){
      print("Error in selection function/No selection function")
    })
  }
  
  combobox<-ttkcombobox(frame,values=options,textvariable=combovar,width=width,justify="center")
  tkgrid(combobox,row=1,column=2,pady=10,padx=10,sticky="w")
  tkbind(combobox,"<<ComboboxSelected>>",get_selection)
  
  combolabel<-tklabel(frame,text=label)
  tkgrid(combolabel,row=1,column=1,pady=10,sticky="e")
  
}

#List of entries
entry_list_frame<-function(frame,labels,widths,defaults){
  
  tryCatch(.GlobalEnv$widths_val<-widths,
           error=function(e){.GlobalEnv$widths_val<-rep(8,times=length(labels))})
  
  tryCatch(.GlobalEnv$defaults_val<-defaults,
           error=function(e){.GlobalEnv$defaults_val<-rep("",times=length(labels))})
  
  if(length(labels)==length(widths)){
 
    scrape_entries<-function(){
      
      .GlobalEnv$user_input<-unlist(lapply(1:length(labels),function(z){
        tclvalue(eval(parse(text=paste("var",z,sep=""))))
      }))
      print(user_input)
      
    }
    
    lapply(1:length(labels),function(x){
      
      tmp_lbl<-labels[x]
      tmp_width<-widths[x]
      
      assign(paste("var",x,sep=""),
             tclVar(defaults_val[x]),envir=.GlobalEnv)
      
      assign(paste("lbl",x,sep=""),
             tklabel(frame,text=tmp_lbl,font=norm_font),
             envir=.GlobalEnv)
      tkgrid(eval(parse(text=paste("lbl",x,sep=""))),row=x,column=1,sticky="e")
      
      assign(paste("entry",x,sep=""),
             tkentry(frame,width=tmp_width,justify="center",textvariable=eval(parse(text=paste("var",x,sep="")))),
             envir=.GlobalEnv)
      tkgrid(eval(parse(text=paste("entry",x,sep=""))),row=x,column=2,padx=10,pady=3,sticky="w")
      tkbind(eval(parse(text=paste("entry",x,sep=""))),"<Return>",scrape_entries)
      
    })
    
    submit_but<-tkbutton(frame,text="Submit",font=norm_font,command=scrape_entries)
    tkgrid(submit_but,row=(length(labels)+1),column=1,columnspan=2,pady=10)
    
  } else{
    print("Labels and widths must be the same length!")
  }
  
}



