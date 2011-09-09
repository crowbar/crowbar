/**
 * Multi-Select, Drag And Drop
 * Work on all browser platform including IE6
 * Version Alpha 0.1
 * Its a plugin that combine all the jquery-ui (Drag,Drop,Select);
 * You need the custom $.UI to be able to use this
 * You need to add this to your style-sheet
 * .ui-selectable-helper {}
 * .ui-selectable-helper div {}
 * There's no major change in $.UI since its just to adapt some of the plugins
 * Please leave me some comment on anything!
 * 
 * Example :
 * $('{container}').drag_drop_multi_select({
 *     {setting}:'{value}'
 * });
 *
 * Copyright (c) 2010 Samuel Cléroux-Bouthillier
 *
 * Dual licensed under MIT and GNU General Public License version 3 (GPLv3)
 * http://www.opensource.org/licenses
 * Launch : September 2010
 */
(function($){
    var $self;
    $.fn.exists = function(){if(jQuery(this).length == 0){return false;}else{return true;}};
    $.fn.drag_drop_multi_select = function(settings){
        var config = $.fn.drag_drop_multi_select.defaults;
        if (settings) config = $.extend($.fn.drag_drop_multi_select.defaults, settings);
        return this.each(function(i) {
            // DECLARE VAR
            var unique_id = $.fn.drag_drop_multi_select.unique++;
            var dragging = false;

            // DECLARE DEFAULT VALUE
            $self = $(this);
            $self.attr('ddms',unique_id);
            $self.addClass('ddms-container');

            // START KEY CAPTURING
            $.fn.captureKeys();

            // DECLARE STACKS AND SETTING FOR EACH OBJECT
            $.fn.drag_drop_multi_select.stack[unique_id]={"selected":[ ],"all":[ ]};
            $.fn.drag_drop_multi_select.settings[unique_id] = config;

            // BIND BASIC EVENTS TO EACH ELEMENT THAT WILL BE DRAGGABLE
            $self.find(config.element_to_drag_drop_select).each(function(){
                var $item = $(this);
                var item_id = $.fn.drag_drop_multi_select.unique++;

                $item.attr('ddms',item_id);

                $.fn.drag_drop_multi_select.settings['item_'+$item.attr('ddms')] = {self:$item,parent_to_refer:$self};
                $.fn.drag_drop_multi_select.stack[unique_id].all.push(item_id);

                // BIND CLICK ITEM
                $item.bind('click.ddms_item_clicked',function(){
                    if($.fn.keyisPressed(CTRL_KEY)|| $.fn.keyisPressed(SHIFT_KEY)||($.fn.drag_drop_multi_select.stack[$.fn.drag_drop_multi_select.get_instance_id($(this).attr('ddms'))].selected.length == 1 && $item.hasClass($.fn.drag_drop_multi_select.settings[$.fn.drag_drop_multi_select.get_instance_id($item.attr('ddms'))].selectedClass))){
                        $.fn.drag_drop_multi_select.toggle(item_id);//CTRLKEY IS DOWN - ADD ITEM TO SELECTION
                    }else{
                        $.fn.drag_drop_multi_select.replace(item_id);//CTRLKEY IS NOT DOWN - MAKE NEW SELECTION
                    }
                });

                $item.bind('ddms.select',function(){
                    $(this).addClass('ui-selected');
                    $(this).addClass($.fn.drag_drop_multi_select.settings[$.fn.drag_drop_multi_select.get_instance_id($(this).attr('ddms'))].selectedClass);
                    if($self.find('.ui-selected').size() == 0)
                    $(config.element_to_show_result).text('');
                    else
                    $(config.element_to_show_result).text($self.find('.ui-selected').size()+" items are selected");
                });

                $item.bind('ddms.deselect',function(){
                    $(this).removeClass('ui-selected');
                    $(this).removeClass($.fn.drag_drop_multi_select.settings[$.fn.drag_drop_multi_select.get_instance_id($(this).attr('ddms'))].selectedClass);
                    if($self.find('.ui-selected').size() == 0)
                    $(config.element_to_show_result).text('');
                    else
                    $(config.element_to_show_result).text($self.find('.ui-selected').size()+" items are selected");
                });
            });

            // MAKE ITEMS SELECTABLE
            $self.selectable({
                cancel: "'"+config.elements_to_cancel_select+","+config.element_to_drag_drop_select+"'",
                filter: $self.find(config.element_to_drag_drop_select),
                selecting: function(ev,ui) {
                    if($self.find('.ui-selecting').size() == 0)
                    $(config.element_to_show_result).text('');
                    else
                    $(config.element_to_show_result).text($self.find('.ui-selecting').size()+" items are in the selection");
                },
                unselecting: function(ev,ui) {
                    if($self.find('.ui-selecting').size() == 0)
                    $(config.element_to_show_result).text('');
                    else
                    $(config.element_to_show_result).text($self.find('.ui-selecting').size()+" items are in the selection");
                },
                stop: function(ev,ui) {
                    if($self.find('.ui-selected').size() == 0)
                    $(config.element_to_show_result).text('');
                    else
                    $(config.element_to_show_result).text($self.find('.ui-selected').size()+" items are selected");
                },
                selected: function(ev,ui) {
                    $.fn.drag_drop_multi_select.select(parseInt($(ui.selected).attr('ddms')));
                },
                unselected: function(ev,ui) {
                    $.fn.drag_drop_multi_select.deselect(parseInt($(ui.unselected).attr('ddms')));
                }
            });

            // MAKE ITEMS DRAGGABLE
            $self.find(config.element_to_drag_drop_select).draggable({
                helper:function(){
                   $clicked_item = $(this);
                   if(!$clicked_item.hasClass('ddms_selected') ){
                       //TRIGGER THE CLICK
                       $clicked_item.trigger('click.ddms_item_clicked');
                   }
                   // CREATE CLONE HELPER
                   var instance_id = $.fn.drag_drop_multi_select.get_instance_id($clicked_item.attr('ddms'));
                   var $helper = $('<div ddms_instance="'+instance_id+'"></div>').append('<div class="inside_helper" style="position:absolute;top:-'+$.fn.drag_drop_multi_select.getTopOffset( $clicked_item )+'px;left:-'+$.fn.drag_drop_multi_select.getLeftOffset( $clicked_item )+'px;" ></div>');
                   $helper.find('.inside_helper').append($.fn.drag_drop_multi_select.getSelectedForDragging( $clicked_item.attr('ddms')));
                   $.fn.drag_drop_multi_select.getListItems(instance_id).filter($.fn.drag_drop_multi_select.settings[instance_id].selectedClass).addClass($.fn.drag_drop_multi_select.settings[instance_id].ghostClass);
                   return $helper;
                },
                cursor:'move',
                revert:'invalid',
                scroll: false,
                start:function(e,ui){
                    dragging = true;
                },
                drag:function(e,ui){

                },
                stop:function(e,ui){
                   $clicked_item = $(this);
                   var instance_id = $.fn.drag_drop_multi_select.get_instance_id($clicked_item.attr('ddms'));
                   $.fn.drag_drop_multi_select.getListItems(instance_id).filter($.fn.drag_drop_multi_select.settings[instance_id].selectedClass).removeClass($.fn.drag_drop_multi_select.settings[instance_id].ghostClass);
                }
            });

            // MAKE ITEMS DROPPABLE
            var array_of_selected = [];
            var found = false;
            $(config.elements_to_drop).droppable({
                tolerance:'pointer',
                over: function(e,ui) {
                    var $target = $(e.target);
                    var old_instance = parseInt(ui.helper.attr('ddms_instance'));
                    found = false;
                    array_of_selected = [];

                    $.fn.drag_drop_multi_select.getListItems(old_instance).filter(config.selectedClass).each(function(){
                        array_of_selected.push($(this).attr('ddms'));
                    });

                    if($.inArray($target.attr('ddms'),array_of_selected) > -1) {
                        found = true;
                    }

                    $target.addClass($.fn.drag_drop_multi_select.settings[unique_id].hoverClass);
                },
                out: function(e,ui) {
                    var $target = $(e.target);
                    $target.removeClass($.fn.drag_drop_multi_select.settings[unique_id].hoverClass);
                },
                drop:function(e,ui){
                    var $target = $(e.target);
                    $target.removeClass($.fn.drag_drop_multi_select.settings[unique_id].hoverClass);

                    var old_instance = parseInt(ui.helper.attr('ddms_instance'));

                    ui.helper.find('.inside_helper .'+config.selectedClass).each(function(){
                        var item_id = parseInt( $(this).attr('ddms_drag'));
                        var $item_instance = $.fn.drag_drop_multi_select.settings['item_'+item_id].self;
                        $.fn.drag_drop_multi_select.moveBetweenLists( item_id, $item_instance, old_instance, unique_id, e, ui);
                    });
                }
            });
                });
    };

    // Helper function to clone items after the fact. Useful for shopping cart-like entries.
    $.fn.drag_drop_multi_select_clone=function(item) {
      var items = item.clone();

      var self = item.first().closest(".ddms-container");
      var unique_id = self.attr('ddms');
      var config = $.fn.drag_drop_multi_select.settings[unique_id];

      items.each(function(){
        var $item = $(this);
        var item_id = $.fn.drag_drop_multi_select.unique++;

        $item.attr('ddms',item_id);

        $.fn.drag_drop_multi_select.settings['item_'+$item.attr('ddms')] = {self:$item,parent_to_refer:$self};
        $.fn.drag_drop_multi_select.stack[unique_id].all.push(item_id);

        // BIND CLICK ITEM
        $item.bind('click.ddms_item_clicked',function(){
           if($.fn.keyisPressed(CTRL_KEY)|| $.fn.keyisPressed(SHIFT_KEY)||($.fn.drag_drop_multi_select.stack[$.fn.drag_drop_multi_select.get_instance_id($(this).attr('ddms'))].selected.length == 1 && $item.hasClass($.fn.drag_drop_multi_select.settings[$.fn.drag_drop_multi_select.get_instance_id($item.attr('ddms'))].selectedClass))){
             $.fn.drag_drop_multi_select.toggle(item_id);//CTRLKEY IS DOWN - ADD ITEM TO SELECTION
           }else{ 
             $.fn.drag_drop_multi_select.replace(item_id);//CTRLKEY IS NOT DOWN - MAKE NEW SELECTION
           }
         });
 
         $item.bind('ddms.select',function(){
           $(this).addClass('ui-selected');
           $(this).addClass($.fn.drag_drop_multi_select.settings[$.fn.drag_drop_multi_select.get_instance_id($(this).attr('ddms'))].selectedClass);
           if($self.find('.ui-selected').size() == 0)
             $(config.element_to_show_result).text('');
           else
             $(config.element_to_show_result).text($self.find('.ui-selected').size()+" items are selected");
         });
 
         $item.bind('ddms.deselect',function(){
           $(this).removeClass('ui-selected');
           $(this).removeClass($.fn.drag_drop_multi_select.settings[$.fn.drag_drop_multi_select.get_instance_id($(this).attr('ddms'))].selectedClass);
           if($self.find('.ui-selected').size() == 0)
             $(config.element_to_show_result).text('');
           else
             $(config.element_to_show_result).text($self.find('.ui-selected').size()+" items are selected");
         });
      });

      // MAKE ITEMS DRAGGABLE
      items.draggable({
          helper:function(){
             $clicked_item = $(this);
             if(!$clicked_item.hasClass('ddms_selected') ){
                 //TRIGGER THE CLICK
                 $clicked_item.trigger('click.ddms_item_clicked');
             }
             // CREATE CLONE HELPER
             var instance_id = $.fn.drag_drop_multi_select.get_instance_id($clicked_item.attr('ddms'));
             var $helper = $('<div ddms_instance="'+instance_id+'"></div>').append('<div class="inside_helper" style="position:absolute;top:-'+$.fn.drag_drop_multi_select.getTopOffset( $clicked_item )+'px;left:-'+$.fn.drag_drop_multi_select.getLeftOffset( $clicked_item )+'px;" ></div>');
             $helper.find('.inside_helper').append($.fn.drag_drop_multi_select.getSelectedForDragging( $clicked_item.attr('ddms')));
             $.fn.drag_drop_multi_select.getListItems(instance_id).filter($.fn.drag_drop_multi_select.settings[instance_id].selectedClass).addClass($.fn.drag_drop_multi_select.settings[instance_id].ghostClass);
             return $helper;
          },
          cursor:'move',
          revert:'invalid',
          scroll: false,
          start:function(e,ui){
              dragging = true;
          },
          drag:function(e,ui){

          },
          stop:function(e,ui){
             $clicked_item = $(this);
             var instance_id = $.fn.drag_drop_multi_select.get_instance_id($clicked_item.attr('ddms'));
             $.fn.drag_drop_multi_select.getListItems(instance_id).filter($.fn.drag_drop_multi_select.settings[instance_id].selectedClass).removeClass($.fn.drag_drop_multi_select.settings[instance_id].ghostClass);
          }
      });

      return items;
    };

    // DEFAULTS SETTINGS OF PLUGINS
    $.fn.drag_drop_multi_select.defaults={
        element_to_drag_drop_select:'.ddms', // element that will be draggable,selectable,droppable
        elements_to_cancel_select:'.cancel',// element that you dont want to do selectable
        elements_to_drop:'.drop',// element that you can drop on it
        element_to_show_result:'.items_selected', // element that will be showing how much are selected
        moveOpacity: 0.5, //opacity of moving items
        ghostClass: 'ddms_ghost', //class for "left-behind" item.
        hoverClass: 'ddms_hover', //class for acceptable drop targets on hover
        moveClass:  'ddms_move', //class to apply to items when moving them.
        selectedClass: 'ddms_selected', // class to apply to items that will be select.
        after_drop_action:function($item_instance,$old_container,$new_container,event,helper){}
    };
    $.fn.drag_drop_multi_select.unique=0;
    $.fn.drag_drop_multi_select.stack=[];
    $.fn.drag_drop_multi_select.settings=[];

    // FUNCTION TO EXECUTE AFTER A DROP
    $.fn.drag_drop_multi_select.moveBetweenLists=function(item_id, $item_instance, old_container, new_container, event, helper){
        var instance_id = $.fn.drag_drop_multi_select.get_instance_id(item_id);
        // DESELECT ITEMS
        $.fn.drag_drop_multi_select.deselect(parseInt(item_id));
        $($.fn.drag_drop_multi_select.settings[instance_id].element_to_show_result).text('');
        // DECLAIRE VARIABLES TO SEND TO AFTER DROP FUNCTION
        var $old_container = $('[ddms='+old_container+']');
        var $new_container = $('[ddms='+new_container+']');
        // CALL THE AFTER DROP ACTION FUNCTION
        $.fn.drag_drop_multi_select.settings[instance_id].after_drop_action($item_instance,$old_container,$new_container,event,helper);
        //REMOVING FROM OLD CONTAINER STACK
        $.fn.drag_drop_multi_select.stack[old_container].all.splice( $.inArray( parseInt(item_id),$.fn.drag_drop_multi_select.stack[old_container].all ),1);
        //ADDING TO THE NEW CONTAINER STACK
        $.fn.drag_drop_multi_select.stack[new_container].all.push(parseInt(item_id));
    };

    // GET CLONE AND ALL ITEM THAT WILL BE DRAG
    $.fn.drag_drop_multi_select.getSelectedForDragging=function(item_id){
        var instance_id = $.fn.drag_drop_multi_select.get_instance_id(item_id);
        var $clone_list = $.fn.drag_drop_multi_select.getListItems(instance_id).clone().each(function(){
            var id = $(this).attr('ddms');
            $(this).not("."+$.fn.drag_drop_multi_select.settings[instance_id].selectedClass).hide();
            $(this).filter("."+$.fn.drag_drop_multi_select.settings[instance_id].selectedClass).addClass($.fn.drag_drop_multi_select.settings[instance_id].moveClass).css({opacity:$.fn.drag_drop_multi_select.settings[instance_id].moveOpacity});
            $(this).attr('ddms_drag',$(this).attr('ddms'));
            $(this).attr('ddms','');
            $(this).css({'position':'absolute','left':$('[ddms="'+id+'"]').position().left,'top':$('[ddms="'+id+'"]').position().top});
        });
        return $clone_list;
    };

    // GET TOP OFFSET OF ITEMS CLIKED
    $.fn.drag_drop_multi_select.getTopOffset=function($item){
        //find this items offset and the first items offset.
        var this_offset = $item.position().top;
        var first_offset = $.fn.drag_drop_multi_select.getListItems($.fn.drag_drop_multi_select.get_instance_id($item.attr('ddms'))).eq(0).position().top;
        return this_offset;
    };

    // GET LEFT OFFSET OF ITEMS CLIKED
    $.fn.drag_drop_multi_select.getLeftOffset=function($item){
        //find this items offset and the first items offset.
        var this_offset = $item.position().left;
        var first_offset = $.fn.drag_drop_multi_select.getListItems($.fn.drag_drop_multi_select.get_instance_id($item.attr('ddms'))).eq(0).position().left;
        return this_offset;
    };

    // TOGGLE THE SELECT AND DESELECT FUNCTIONS
    $.fn.drag_drop_multi_select.toggle=function(item_id){
        if(!$.fn.drag_drop_multi_select.isSelected(item_id)){
            $.fn.drag_drop_multi_select.select(item_id);
        }else {
            $.fn.drag_drop_multi_select.deselect(item_id);
        }
    };

    // FUNCTION THAT SELECT ELEMENT IF ITS NOT ALREADY SELECTED AND ADD IT TO THE STACK OF SELECTED
    $.fn.drag_drop_multi_select.select=function(item_id){
        var instance_id = $.fn.drag_drop_multi_select.get_instance_id(item_id);
        var $item_instance = $.fn.drag_drop_multi_select.settings['item_'+item_id].self;

        if(!$.fn.drag_drop_multi_select.isSelected(item_id)){
            $.fn.drag_drop_multi_select.stack[instance_id].selected.push(item_id);
            $item_instance.trigger('ddms.select');
        }
    };

    // FUNCTION THAT DESELECT ELEMENT FROM STACK
    $.fn.drag_drop_multi_select.deselect=function(item_id){
        var instance_id = $.fn.drag_drop_multi_select.get_instance_id(item_id);
        var $item_instance = $.fn.drag_drop_multi_select.settings['item_'+item_id].self;

        if($.fn.drag_drop_multi_select.isSelected(item_id)){
            $.fn.drag_drop_multi_select.stack[instance_id].selected.splice($.inArray(item_id,$.fn.drag_drop_multi_select.stack[instance_id].selected),1);
            $item_instance.trigger('ddms.deselect');
        }
    };

    // FUNCTION THAT CHECK IF ELEMENT IS ALREADY IN STACK
    $.fn.drag_drop_multi_select.isSelected=function(item_id){
        var $item_instance = $.fn.drag_drop_multi_select.settings['item_'+item_id].self;
        var instance_id = $.fn.drag_drop_multi_select.get_instance_id(item_id);
        return $item_instance.hasClass($.fn.drag_drop_multi_select.settings[instance_id].selectedClass);
    };

    // FUNCTION THAT EMPTY THE STACK AND START A NEW ONE
    $.fn.drag_drop_multi_select.replace=function(item_id){
        var instance_id = $.fn.drag_drop_multi_select.get_instance_id(item_id);
        $.fn.drag_drop_multi_select.selectNone(instance_id);
        $.fn.drag_drop_multi_select.stack[instance_id].selected.push(item_id);
        var $item_instance = $.fn.drag_drop_multi_select.settings['item_'+item_id].self;
        $item_instance.trigger('ddms.select');
    };

    // FUNCTION THAT GET ALL ELEMENT THAT ARE IN A STACK
    $.fn.drag_drop_multi_select.getListItems=function(instance_id){
        var element_to_drag_drop_select = $.fn.drag_drop_multi_select.settings[instance_id].element_to_drag_drop_select;
        return $('[ddms='+instance_id+'] '+element_to_drag_drop_select);
    };

    // FUNCTION THAT RETURN INSTANCE ID OF THE SELECTED ITEM
    $.fn.drag_drop_multi_select.get_instance_id=function(item_id){
        return parseInt($.fn.drag_drop_multi_select.settings['item_'+item_id].parent_to_refer.eq(0).attr('ddms'));
    };

    // FUNCTION THAT SERIALIZE AN ARRAY FROM THE RESULTS
    $.fn.drag_drop_multi_select.serializeArray=function( list_id ){
        var out = [];
        $.fn.drag_drop_multi_select.getListItems(list_id).each(function(){
            out.push($(this).attr('id'));
        });
        return out;
    };
    $.fn.drag_drop_multi_select.serialize=function( list_id ){
            return $.fn.drag_drop_multi_select.serializeArray( list_id ).join(", ");
    };

    // FUNCTION THAT DESELECT ALL ELEMENT IN A STACK
    $.fn.drag_drop_multi_select.selectNone=function(instance_id){
        $.fn.drag_drop_multi_select.getListItems(instance_id).each(function(){
            $.fn.drag_drop_multi_select.deselect( $(this).attr('ddms') );
        });
        return false;
    };

    // FUNCTION THAT SELECT ALL ELEMENT IN A STACK
    $.fn.drag_drop_multi_select.selectAll=function(list_id){
        $.fn.drag_drop_multi_select.getListItems(list_id).each(function(){
            $.fn.drag_drop_multi_select.select( $(this).attr('ddms') );
        });
        return false;
    };

    // EXTEND TO ELEMENT SOME FUNCTION
    $.extend({
        ddms:{
            selectAll:function(id){ return $.fn.drag_drop_multi_select.selectAll($('#'+id).attr('ddms')); },
            selectNone:function(id){ return $.fn.drag_drop_multi_select.selectNone($('#'+id).attr('ddms')); },
            serialize:function(id){ return $.fn.drag_drop_multi_select.serialize($('#'+id).attr('ddms')); }
        }
    });

    // KEY CAPTURING DEFAULT KEYCODE
    var CTRL_KEY = 17;
    var ALT_KEY = 18;
    var SHIFT_KEY = 16;

    // FUNCTION THAT CAPTURE KEY AND CHECK WHAT IS DOWN
    $.fn.captureKeys=function(){
        if($.fn.captureKeys.capturing){ return; }
        $(document).keydown(function(e){
            if(e.keyCode == CTRL_KEY ){$.fn.captureKeys.stack.CTRL_KEY  = true}
            if(e.keyCode == SHIFT_KEY){$.fn.captureKeys.stack.SHIFT_KEY = true}
            if(e.keyCode == ALT_KEY  ){$.fn.captureKeys.stack.ALT_KEY   = true}
        });
        $(document).keyup(function(e){
            if(e.keyCode == CTRL_KEY ){$.fn.captureKeys.stack.CTRL_KEY  = false}
            if(e.keyCode == SHIFT_KEY){$.fn.captureKeys.stack.SHIFT_KEY = false}
            if(e.keyCode == ALT_KEY  ){$.fn.captureKeys.stack.ALT_KEY   = false}
        });

    };
    $.fn.captureKeys.stack={CTRL_KEY:false, SHIFT_KEY:false, ALT_KEY:false};
    $.fn.captureKeys.capturing=false;
    $.fn.keyisPressed=function(key){
        switch(key){
            case  CTRL_KEY: return $.fn.captureKeys.stack.CTRL_KEY;
            case   ALT_KEY: return $.fn.captureKeys.stack.ALT_KEY;
            case SHIFT_KEY: return $.fn.captureKeys.stack.SHIFT_KEY;
            default: return false;
        }
    }
})(jQuery);

