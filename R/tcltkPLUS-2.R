
# ---------------------------------CREATE GENERAL FUNCTIONS FOR PACKAGE

#' @param pos defaults to 1 which equals an assingment to global environment

#Save binding events
binding_events<-data.frame(
  Widget=c(rep("GLOBAL",times=11),"tcListbox","tcCombobox"),
  Event=c("<Button>",
          "<Button-1>",
          "<Button-2>",
          "<Button-3>",
          "<Motion>",
          "<ButtonRelease>",
          "<Double-Button>",
          "<Enter>",
          "<Leave>",
          "<Return>",
          "<Key>",
          "<<ListboxSelect>>",
          "<<ComboboxSelected>>"),
  Description=c("A mouse button is pressed",
                "The left mouse button is pressed",
                "The middle mouse button is pressed",
                "The right mouse button is pressed",
                "A mouse button is pressed and the cursor is moved simultaneously",
                "A mouse button is released",
                "A mouse button is double clicked",
                "The cursor entered a widget",
                "The cursor left a widget",
                "The Enter key is pressed",
                "Any key is pressed",
                "A listbox element is selected",
                "A combobox element is selected")
)

common_functions<-data.frame(
  Syntax=c("tcl('update')",
           "tk_messageBox()",
           "tk_select.list()",
           "tk_choose.files()",
           "tk_choose.dir()"),
  Purpose=c("Forces an update of the tcl window.",
            "Creates a prompt with a message and returns the user's response",
            "Creates a prompt with a list of options and returns the user's selection.",
            "Opens a file selection window.",
            "Opens a directory selection window."),
  More=c("NA",
         "?tk_messageBox",
         "?tk_select.list",
         "?tk_choose.files",
         "?tk_choose.dir")
)

#Define themes
DEFAULT<-list(
  Background="gray95",
  WidgetBackground="gray95",
  TextColor="black",
  TextSize=10,
  TextFamily="Arial"
)
LARGE<-list(
  Background="gray95",
  WidgetBackground="gray95",
  TextColor="black",
  TextSize=15,
  TextFamily="Arial"
)
SMALL<-list(
  Background="gray95",
  WidgetBackground="gray95",
  TextColor="black",
  TextSize=8,
  TextFamily="Arial"
)
RETRO<-list(
  Background="violet",
  WidgetBackground="purple",
  TextColor="yellow3",
  TextSize=12,
  TextFamily="Comic Sans MS"
)
DARK<-list(
  Background="gray20",
  WidgetBackground="gray20",
  TextColor="gray60",
  TextSize=10,
  TextFamily="Arial"
)

# Parse position (for building ui)
#' @keywords internal
#' @NoRd
grid_position<-function(widget,position,propogate=TRUE){

  tkgrid(get(widget),
         column=position[1],
         row=position[2],
         padx=position[3],
         pady=position[4],
         columnspan=position[5],
         rowspan=position[6],
         sticky=position[7])
  tkgrid.propagate(get(widget),propogate)

}

# Build a binding string
#' @keywords internal
#' @NoRd
assign_bind<-function(widget,binding){

  if(length(which(binding_events==binding[1]))>0){
    tkbind(get(widget),
           binding[1],
           get(binding[2]))
  } else{
    print(paste("The binding event ",binding[1]," is not recognized for ",widget,". Use get_events() to see all events.",sep=""))
  }

}

# Curate names
#' @keywords internal
#' @NoRd
cur_name<-function(name){
  if(name!=""){
    out<-paste(name,"<-",sep="")
  } else{
    out<-""
  }
  return(out)
}

# Define variable from widget
#' @keywords internal
#' @NoRd
define_widget_var<-function(widget_name){
  return(paste(widget_name,"_TCVAR",sep=""))
}

#Parse widgets from widget list
#' @keywords internal
#' @NoRd
parse_widgets<-function(x,widget_list,pos=1){

  #Set name to custom name if detected
  if(str_detect(widget_list[[x]]$Widget,"<-")==TRUE){
    widget<-str_sub(widget_list[[x]]$Widget,end=str_locate(widget_list[[x]]$Widget,"<-")[[1]]-1)
  } else{
    #Create current widget object
    widget<-paste("widget",x,sep="")
  }

  #If there are values to preserve then preserve
  if(length(widget_list[[x]]$Preserve)>0){

    for(i in seq_along(widget_list[[x]]$Preserve)){

      val_name<-names(widget_list[[x]]$Preserve[i])
      vals<-widget_list[[x]]$Preserve[[i]]

      val_text<-paste("c(",paste("'",vals,"'",collapse=",",sep=''),")",sep="")
      new_widget_text<-gsub(val_name,val_text,widget_list[[x]]$Widget)

      widget_list[[x]]$Widget<-new_widget_text

    }

  }

  #If values to convert, then convert ans save as variable
  if(length(widget_list[[x]]$Convert)>0){

    for(i in seq_along(widget_list[[x]]$Convert)){

      #Retrive values to convert
      val_name<-names(widget_list[[x]]$Convert[i])
      vals<-widget_list[[x]]$Convert[[i]]

      #Create tclVar
      var_name<-define_widget_var(widget_list[[x]]$Name)
      assign(var_name,
             tclVar(vals),
             envir=as.environment(pos)
      )
      new_widget_text<-gsub(val_name,var_name,widget_list[[x]]$Widget)

      widget_list[[x]]$Widget<-new_widget_text

    }

  }

  #If there are other parms
  if(length(widget_list[[x]]$Other)>0){

    if(widget_list[[x]]$Class=="Listbox"){

      if(widget_list[[x]]$Other$Scrollbar==TRUE){
        #Add a scrollbar
        scroll_widget<-paste("scrollwidget",x,sep="")
        scroll_parent<-str_sub(widget_list[[x]]$Widget,
                               start=str_locate(widget_list[[x]]$Widget,"parent=")[[2]]+1,
                               end=str_locate(widget_list[[x]]$Widget,",")[[1]]-1)

        assign(scroll_widget,
               tkscrollbar(get(scroll_parent),command=function(...) tkyview(get(widget),...)),
               envir=as.environment(pos))

        scroll_pos<-c(widget_list[[x]]$Position[1:6],"nws")
        scroll_pos[3]<-max(c(as.numeric(scroll_pos[3])-17,0))

        #Define widget position
        grid_position(scroll_widget,
                      scroll_pos)

        #Update listbox text
        scroll_fun<-paste("tkset(",scroll_widget,",...)",sep="")
        widget_list[[x]]$Widget<-str_replace(widget_list[[x]]$Widget,
                                             "return\\(NULL\\)",
                                             scroll_fun)

      }

    } else if(widget_list[[x]]$Class=="Plot"){

      if(widget_list[[x]]$Other$FRAME==TRUE){

        # #Create a frame for the plot with propogate = FALSE
        plot_frame<-paste("plotframe",x,sep="")
        frame_parent<-str_sub(widget_list[[x]]$Widget,
                              start=str_locate(widget_list[[x]]$Widget,"parent=")[[2]]+1,
                              end=str_locate(widget_list[[x]]$Widget,",")[[1]]-1)

        assign(plot_frame,
               tkframe(get(frame_parent),
                       width=257*widget_list[[x]]$Other$SCALE[1],
                       height=257*widget_list[[x]]$Other$SCALE[2]),
               envir=as.environment(pos))
        #Define widget position
        grid_position(plot_frame,
                      widget_list[[x]]$Position,
                      propogate = FALSE)

        #Overwrite existing frame
        widget_list[[x]]$Widget<-str_replace(widget_list[[x]]$Widget,frame_parent,plot_frame)
      }

    }

  }

  # ADD WIDGETS

  #Define what the widget is by widget list
  #Adds widget to frame
  assign(widget,
         eval(parse(text=widget_list[[x]]$Widget)),
         envir=as.environment(pos)
  )

  propogate_status<-ifelse(length(widget_list[[x]]$Other$PROPOGATE)>0,widget_list[[x]]$Other$PROPOGATE,FALSE)

  #Define widget position
  grid_position(widget,
                widget_list[[x]]$Position,
                propogate = propogate_status)

  #Define widget binding (if applicable)
  if(length(widget_list[[x]]$Binding)>0){
    assign_bind(widget,
                widget_list[[x]]$Binding)
  }

}

# ---------------------------------CREATE USER ASSIST FUNCTIONS

# Get all binding events
#' @export get_common
get_common<-function(){

  print(common_functions)

}

# Get all binding events
#' @export get_events
get_events<-function(){

  print(binding_events)

}

# ---------------------------UI MODIFICATION FUNCTIONS

#Set a theme
#' @export tcTheme
tcTheme<-function(theme_name="DEFAULT"){
  .GlobalEnv$tcCurrentTheme<-get(theme_name)
}
tcCurrentTheme<-get("DEFAULT")

#Extract data from a widget
#' @export tcGet
tcGet<-function(widget_name,widget_list){

  all_names<-unlist(lapply(seq_along(widget_list),function(i){widget_list[[i]]$Name}))

  if(length(which(all_names==widget_name))==1){
    sel_widget<-widget_list[[which(all_names==widget_name)]]

    if(sel_widget$Class=="Entry"){

      return(tclvalue(get(define_widget_var(sel_widget$Name))))

    } else if(sel_widget$Class=="Combobox"){

      return(tclvalue(get(define_widget_var(sel_widget$Name))))

    } else if(sel_widget$Class=="Listbox"){

      selected_ind<-as.numeric(tkcurselection(get(sel_widget$Name)))+1
      return(sel_widget$Convert$VALUES[selected_ind])

    } else if(sel_widget$Class=="Checkbutton"){

      return(tclvalue(get(define_widget_var(sel_widget$Name))))

    } else if(sel_widget$Class=="Scrollbar"){
      return(tclvalue(get(define_widget_var(sel_widget$Name))))
    }
  } else{
    print(paste("Improper widget name: ",widget_name,". Returned ",as.character(length(which(all_names==widget_name)))," matches in widget list!",sep=""))
  }



}

# Build binding vector
#' @export tcBind
tcBind<-function(event,
                 fun){

  if(length(which(binding_events==event))>0){

    out<-c(event,fun)
    return(out)

  } else{
    print(paste("The binding event ",event," is not recognized.",sep=""))
    return(event)
  }


}

# Build position vector
#' @export tcGrid
tcGrid<-function(x,
                 y,
                 padx=0,
                 pady=0,
                 xspan=1,
                 yspan=1,
                 sticky=""){

  out<-c(x,y,padx,pady,xspan,yspan,sticky)
  return(out)
}

#Create a border vector
#' @export tcBorder
tcBorder<-function(width,
                   color){

  out<-c(width,color)
  return(out)

}

#Replace an existing widget with a new one
#' @export tcReplace
tcReplace<-function(old_widget_name,
                    widget_list,
                    new_widget_object){

  if(is(get(old_widget_name),"tkwin")==TRUE){
    #Destroy the old widget
    tkdestroy(get(old_widget_name))

    #Get old widget names
    widget_ind<-which(lapply(widget_list,"[[",2)==old_widget_name)

    #Update widget list
    widget_list[[widget_ind]]<-new_widget_object

    #Save widget list to parent environment
    assign("widget_list",
           widget_list,
           envir=parent.frame())

    #Create the new widget
    parse_widgets(widget_ind,widget_list)
  } else{
    print(paste("Widget ",old_widget_name," not found!",sep=""))
  }


}

#Modify an existing widget
#' @export tcModify
tcModify<-function(widget_name,
                   widget_list,
                    ...){

  #Name of new argument is the argument to change, value is the new value
  new_argument<-list(...)
  .GlobalEnv$new_argument<-list(...)

  # Get index of the current widget
  widget_ind<-which(lapply(widget_list,"[[",2)==widget_name)

  #Find the old value in the widget based on the position of the argument
  old_value_position<-str_locate_all(widget_list[[widget_ind]]$Widget,paste("(?<=",names(new_argument),"=)(.*?)(?=,|\\))",sep=""))[[1]]

  #If the argument isnt in the widget text...
  if(widget_list[[widget_ind]]$Class=="Listbox"&&names(new_argument)=="values"){

    #if you are changing the values of the listbox
    if(names(new_argument)=="values"){
      widget_list[[widget_ind]]$Convert$VALUES<-new_argument[[1]]
    }

  } else if(widget_list[[widget_ind]]$Class=="Combobox"&&names(new_argument)%in%c("values","selected")){

    #if you are changing the values of the combobox
    if(names(new_argument)=="values"){
      widget_list[[widget_ind]]$Preserve$VALUES<-new_argument[[1]]
    } else if(names(new_argument)=="selected"){
      widget_list[[widget_ind]]$Convert$VALUES<-new_argument[[1]]
    }

  } else if(widget_list[[widget_ind]]$Class=="Plot"&&names(new_argument)=="ggplot"){

    old_value_position<-str_locate_all(widget_list[[widget_ind]]$Widget,"(?<=\\{plot\\()(.*?)(?=\\)\\})")[[1]]

    #Replace the old value with the new value (by position not string match)
    str_sub(widget_list[[widget_ind]]$Widget,start=old_value_position[[1]],end=old_value_position[[2]])<-new_argument[[1]]

  } else{

    # Get the value to be replaced
    raw_old_value<-str_sub(widget_list[[widget_ind]]$Widget,start=old_value_position[[1]],end=old_value_position[[2]])

    #Curate the new value
    new_value<-tryCatch(as.numeric(new_argument[[1]]),warning=function(w){return(paste("'",new_argument[[1]],"'",sep=""))})

    #Replace the old value with the new value (by position not string match)
    str_sub(widget_list[[widget_ind]]$Widget,start=old_value_position[[1]],end=old_value_position[[2]])<-new_value

  }

  #Save widget list to parent environment
  assign("widget_list",
         widget_list,
         envir=parent.frame())

  #Remove the old widget
  tcDestroy(widget_name)

  #Parse new widget
  parse_widgets(widget_ind,widget_list)

}

#Add a new widget
#' @export tcAdd
tcAdd<-function(new_widget_object,
                widget_list){

  #Update widget list
  widget_list[[length(widget_list)+1]]<-new_widget_object

  #Save widget list to parent environment
  assign("widget_list",
         widget_list,
         envir=parent.frame())

  #Parse this single widget
  parse_widgets(length(widget_list),widget_list)

}

#Configure an existing widget using typical tcltk arguments
#' @export tcConfigure
tcConfigure<-function(widget_name,
                      ...){

  if(is(get(widget_name),"tkwin")==TRUE){

    tkconfigure(get(widget_name),
                ...)
  } else{
    print(paste("Widget ",widget_name," not found!",sep=""))
  }

}

#Destroy a widget
#' @export tcDestroy
tcDestroy<-function(widget_name){
  tkdestroy(get(widget_name))
}

# Build ui function (last one to call)
  # ui_name = string of name
  # geometry = geometry string
  # widget_list = list of ui elements to include
#' @export tcBuild
tcBuild<-function(ui_name,
                  widget_list,
                  geometry="",
                  self_destruct=FALSE,
                  destruct_delay=0.1,
                  pos=1){
  assign(ui_name,
         tktoplevel(),
         envir=as.environment(pos)
         )
  tkwm.geometry(get(ui_name),geometry)
  tkwm.title(get(ui_name),ui_name)

  #Add widgets from widget list
  invisible(lapply(seq_along(widget_list), function(x){

    # PARSE WIDGET LIST ELEMENT
    parse_widgets(x,widget_list)


  }))

  if(self_destruct==FALSE){
    tkwait.window(get(ui_name))
  } else if(self_destruct==TRUE){
    Sys.sleep(destruct_delay)
    tcDestroy(ui_name)
  }

}

#Build stand-alone app
#' @export tcApp
tcApp<-function(app_name,
                ui_name,
                widget_list,
                geometry="",
                app_directory="",
                r_directory="",
                run_app=FALSE,
                prompts=TRUE){

  if(prompts==TRUE){
    prompt<-tk_messageBox(message="All objects in your current R environment will be saved in your app. Ensure that only necessary packages and objects are loaded.\n\nWould you like to proceed?",type="yesno")
  } else{
    prompt<-"yes"
  }

  if(prompt=="yes"){
    #Only prompt for directory if not already specified
    if(dir.exists(app_directory)==FALSE){
      tk_messageBox(message="Select a directory to house your app!")
      app_directory<-tk_choose.dir(caption="Select app directory")
    }

    if(is.na(app_directory)==FALSE){

      #Only prompt for directory if not already specified
      if(file.exists(r_directory)==FALSE){
        tk_messageBox(message="Select an R .exe to echo off of!")
        r_directory<-tk_choose.files(caption="Select R .exe",
                                     filters=matrix(c("R.exe", "R.exe","All files", "*"),
                                                    2, 2, byrow = TRUE))
      }

      if(file.exists(r_directory)==TRUE){

        #Create the app directory
        save_app<-paste(app_directory,"/",app_name,"/",sep="")

        if(dir.exists(save_app)==TRUE){

          if(prompts==TRUE){
            continue<-tk_messageBox(message="An app of the same name already exists in this directory. Would you like to overwrite it?",
                                    type="yesno")
          } else{
            continue<-"yes"
          }

        } else{

          continue<-"yes"

        }

        if(continue=="yes"){

          suppressWarnings(dir.create(save_app))

          #Save all objects currently loaded
          raw_objs<-ls(envir=.GlobalEnv)
          all_objs<-lapply(raw_objs,identity)
          save_objs<-paste(save_app,"objects.Rdata",sep="")
          save(list=unlist(all_objs),file=save_objs)

          #Save just the widgets
          save_widgets<-paste(save_app,"widgets.Rdata",sep="")
          app_widget_list<-widget_list
          save("app_widget_list",file=save_widgets)

          #Create the R file
          #Get all loaded packages
          all_packages<-(.packages())
          packages_string<-paste(paste("library(",all_packages,")",sep=""),collapse="\n")
          save_appfile<-paste(save_app,app_name,".R",sep="")
          r_text<-paste(
            "# Load packages\n",
            packages_string,"\n\n",
            "# Load R data\n",
            "load('",save_objs,"')","\n", #load data
            "# Load widgets\n",
            "load('",save_widgets,"')","\n\n", #load widgets
            "# Build the app\n",
            "tcBuild('",ui_name,"',app_widget_list,'",geometry,"')", #build the ui
            sep=""
          )
          write.table(r_text,
                      file=save_appfile,
                      quote=FALSE,row.names=FALSE,col.names = FALSE)

          #Create the .bat file
          save_batfile<-paste(save_app,app_name,".bat",sep="")
          bat_text<-paste(
            "@echo on\n\"",
            r_directory,"\" CMD BATCH \"",save_appfile,"\"",
            sep=""
          )
          write.table(bat_text,
                      file=save_batfile,
                      quote=FALSE,row.names=FALSE,col.names = FALSE)

          if(run_app==TRUE){
            shell.exec(save_batfile)
          }

        } #if continuing

      }#if .exe chosen

    }#if directory chosen
  }#If say to continue

}

# ---------------------------------BASIC UI ELEMENTS


# All elements have list with components in order of:
  # Widget: the text parsed to create widget
  # Name: the widgets name, tied to tclVars
  # Position: the widgets position
  # Class: the type of widget
  # Binding: any interactive binding events
  # Convert: values to convert to a tclVar
  # Preserve: values to preserve
  #Other: other widget-specific options

# Frame element
#' @export tcFrame
tcFrame<-function(parent,
                  position,
                  name,
                  border="",
                  background=tcCurrentTheme$Background,
                  width=NA,
                  height=NA,
                  binding=NULL){

  if(length(border)>1){
    border_string<-paste(",highlightthickness=",border[1],
                         ",highlightbackground='",border[2],"'",
                         ",highlightcolor='",border[2],"'",
                         sep="")
  } else{
    border_string<-""
  }

  #Check for propogation
  if(is.na(width)==FALSE&&is.na(height)==FALSE){
    propogate<-FALSE
  } else{
    propogate<-TRUE
  }

  #Create text string for widget
  widget_text<-paste(name,"<-tkframe(parent=",parent,
                     border_string,
                     ",background='",background,"'",
                     ",width=",width,
                     ",height=",height,
                    ")",sep="")

  return_list<-list(Widget=widget_text,
                    Name=name,
                    Position=position,
                    Class="Frame",
                    Other=list(PROPOGATE=propogate),
                    Binding=binding)
  return(return_list)

}

# Label element
#' @export tcLabel
tcLabel<-function(parent,
                  position,
                  name,
                  text,
                  color=tcCurrentTheme$TextColor,
                  background=tcCurrentTheme$WidgetBackground,
                  size=tcCurrentTheme$TextSize,
                  weight="normal",
                  family=tcCurrentTheme$TextFamily,
                  binding=NULL){

  #Create text string for widget
  widget_text<-paste(cur_name(name),"tklabel(parent=",parent,
                    ",text='",text,"'",
                    ",fg='",color,"'",
                    ",background='",background,"'",
                    ",font=tkfont.create(size=",size,",family='",family,"',weight='",weight,"')",
                    ")",sep="")

  return_list<-list(Widget=widget_text,
                    Name=name,
                    Position=position,
                    Class="Label",
                    Binding=binding)
  return(return_list)

}

# Button element
#' @export tcButton
tcButton<-function(parent,
                   position,
                   name,
                   text,
                   command,
                   color=tcCurrentTheme$TextColor,
                   background=tcCurrentTheme$WidgetBackground,
                   size=tcCurrentTheme$TextSize,
                   weight="normal",
                   family=tcCurrentTheme$TextFamily,
                   binding=NULL){

  #Create text string for widget
  widget_text<-paste(cur_name(name),"tkbutton(parent=",parent,
                     ",text='",text,"'",
                     ",fg='",color,"'",
                     ",background='",background,"'",
                     ",font=tkfont.create(size=",size,",family='",family,"',weight='",weight,"')",
                     ",command=",command,
                     ")",sep="")

  return_list<-list(Widget=widget_text,
                    Name=name,
                    Position=position,
                    Class="Button",
                    Binding=binding)
  return(return_list)

}

# Checkbox element
#' @export tcCheckbox
tcCheckbox<-function(parent,
                     position,
                     name,
                     text="",
                     value=FALSE,
                     color=tcCurrentTheme$TextColor,
                     background=tcCurrentTheme$WidgetBackground,
                     size=tcCurrentTheme$TextSize,
                     weight="normal",
                     family=tcCurrentTheme$TextFamily,
                     binding=NULL){

  #Create text string for widget
  widget_text<-paste(cur_name(name),"tkcheckbutton(parent=",parent,
                     ",text='",text,"'",
                     ",variable=VALUE",
                     ",fg='",color,"'",
                     ",activebackground='",background,"'",
                     ",activeforeground='",color,"'",
                     ",background='",background,"'",
                     ",font=tkfont.create(size=",size,",family='",family,"',weight='",weight,"')",
                     ")",sep="")

  return_list<-list(Widget=widget_text,
                    Name=name,
                    Position=position,
                    Class="Checkbutton",
                    Binding=binding,
                    Convert=list(VALUE=value))
  return(return_list)

}

# Listbox element
#' @export tcListbox
tcListbox<-function(parent,
                    position,
                    name,
                    values,
                    height=5,
                    width=10,
                    selectmode="single",
                    color=tcCurrentTheme$TextColor,
                    background=tcCurrentTheme$WidgetBackground,
                    size=tcCurrentTheme$TextSize,
                    weight="normal",
                    family=tcCurrentTheme$TextFamily,
                    add_scrollbar=FALSE,
                    binding=NULL){

  #Create text string for widget
  widget_text<-paste(cur_name(name),"tklistbox(parent=",parent,
                     ",listvariable=VALUES",
                     ",height=",height,
                     ",width=",width,
                     ",selectmode='",tolower(selectmode),"'",
                     ",fg='",color,"'",
                     ",background='",background,"'",
                     ",font=tkfont.create(size=",size,",family='",family,"',weight='",weight,"')",
                     ",exportselection=FALSE",
                     ",yscrollcommand=function(...)return(NULL)",
                     ")",sep="")

  return_list<-list(Widget=widget_text,
                    Name=name,
                    Position=position,
                    Class="Listbox",
                    Binding=binding,
                    Convert=list(VALUES=values),
                    Other=list(Scrollbar=add_scrollbar))
  return(return_list)

}

#Entry element
#' @export tcEntry
tcEntry<-function(parent,
                  position,
                  name,
                  text="",
                  width=10,
                  justify="center",
                  color=tcCurrentTheme$TextColor,
                  background="white",
                  size=tcCurrentTheme$TextSize,
                  weight="normal",
                  family=tcCurrentTheme$TextFamily,
                  binding=NULL){

  #Create text string for widget
  widget_text<-paste(cur_name(name),"tkentry(parent=",parent,
                     ",textvariable=TEXT",
                     ",fg='",color,"'",
                     ",background='",background,"'",
                     ",font=tkfont.create(size=",size,",family='",family,"',weight='",weight,"')",
                     ",justify='",justify,"'",
                     ",width=",width,
                     ")",sep="")

  return_list<-list(Widget=widget_text,
                    Name=name,
                    Position=position,
                    Class="Entry",
                    Binding=binding,
                    Convert=list(TEXT=text))
  return(return_list)

}

# Combobox element
#' @export tcCombobox
tcCombobox<-function(parent,
                     position,
                     name,
                     values,
                     selected="",
                     width=10,
                     justify="center",
                     color=tcCurrentTheme$TextColor,
                     size=tcCurrentTheme$TextSize,
                     weight="normal",
                     family=tcCurrentTheme$TextFamily,
                     binding=NULL){

  #Create text string for widget
  widget_text<-paste(cur_name(name),"ttkcombobox(parent=",parent,
                     ",values=VALUES",
                     ",textvariable=SELECTED",
                     ",justify='",justify,"'",
                     ",width=",width,
                     ",foreground='",color,"'",
                     ",font=tkfont.create(size=",size,",family='",family,"',weight='",weight,"')",
                     ")",sep="")

  return_list<-list(Widget=widget_text,
                    Name=name,
                    Position=position,
                    Class="Combobox",
                    Binding=binding,
                    Convert=list(SELECTED=selected),
                    Preserve=list(VALUES=values))
  return(return_list)

}

# plot element
#' @export tcPlot
tcPlot<-function(parent,
                 position,
                 name,
                 ggplot,
                 hscale=1,
                 vscale=1,
                 binding=NULL,
                 own_frame=TRUE){

  #Create text string for widget
  widget_text<-paste(cur_name(name),"tkrplot(parent=",parent,
                     ",fun=function() {plot(",ggplot,")}",
                     ",hscale=",hscale,
                     ",vscale=",vscale,
                     ")",sep="")

  return_list<-list(Widget=widget_text,
                    Name=name,
                    Position=position,
                    Class="Plot",
                    Binding=binding,
                    Other=list(FRAME=own_frame,
                               SCALE=c(hscale,vscale)))
  return(return_list)


}

# scrollbar element
#' @export tcScrollbar
tcScrollbar<-function(parent,
                      position,
                      name,
                      from,
                      to,
                      resolution,
                      command,
                      variable,
                      showvalue=TRUE,
                      sliderlength=30,
                      height=100,
                      width=15,
                      label="",
                      color=tcCurrentTheme$TextColor,
                      size=tcCurrentTheme$TextSize,
                      weight="normal",
                      family=tcCurrentTheme$TextFamily,
                      orient="vertical",
                      background=tcCurrentTheme$WidgetBackground,
                      troughcolor="gray80",
                      binding=NULL){

  #Create text string for widget
  widget_text<-paste(cur_name(name),"tkscale(parent=",parent,
                     ",from=",from,
                     ",to=",to,
                     ",resolution=",resolution,
                     ",variable=VARIABLE",
                     ",command=",command,
                     ",orient='",orient,"'",
                     ",fg='",color,"'",
                     ",font=tkfont.create(size=",size,",family='",family,"',weight='",weight,"')",
                     ",bg='",background,"'",
                     ",highlightcolor='",background,"'",
                     ",highlightbackground='",background,"'",
                     ",activebackground='",background,"'",
                     ",troughcolor='",troughcolor,"'",
                     ",showvalue=",showvalue,
                     ",sliderlength=",sliderlength,
                     ",length=",height,
                     ",width=",width,
                     ",label='",label,"'",
                     ")",sep="")

  return_list<-list(Widget=widget_text,
                    Name=name,
                    Position=position,
                    Class="Scrollbar",
                    Binding=binding,
                    Convert=list(VARIABLE=variable))
  return(return_list)

}

# ---------------------------------ADVANCED UI ELEMENTS

#A pop-up submission box that returns the user submission
#' @export tcSubmissionbox
tcSubmissionbox<-function(label_text,
                          button_text,
                          entry_text="",
                          entry_width=10,
                          geometry="",
                          self_destruct=FALSE,
                          destruct_delay=0.1){

  submissionboxoutput<-NULL
  .GlobalEnv$submissionboxoutput<-NULL

  .GlobalEnv$submitfunction<-function(){
    tcDestroy("submissionboxui")
    .GlobalEnv$submissionboxoutput<-tcGet("submissionboxentry",submissionbox_list)
  }

  submissionbox_list<-list()

  submissionbox_list[[1]]<-tcFrame(parent="submissionboxui",
                                   position=tcGrid(1,1,padx=20,pady=10),
                                   name="submissionboxframe")

  submissionbox_list[[2]]<-tcLabel(parent="submissionboxframe",
                                   position=tcGrid(1,1),
                                   name="submissionlabel",
                                   text=label_text)

  submissionbox_list[[3]]<-tcEntry(parent="submissionboxframe",
                                   position=tcGrid(1,2,pady=5),
                                   name="submissionboxentry",
                                   text=entry_text,
                                   width=entry_width,
                                   binding=tcBind("<Return>","submitfunction"))

  submissionbox_list[[4]]<-tcButton(parent="submissionboxframe",
                                    position=tcGrid(1,3),
                                    name="submissionbutton",
                                    text=button_text,
                                    command="submitfunction")

  tcBuild("submissionboxui",widget_list=submissionbox_list,geometry=geometry,self_destruct=self_destruct,destruct_delay=destruct_delay)

  return(submissionboxoutput)
}






