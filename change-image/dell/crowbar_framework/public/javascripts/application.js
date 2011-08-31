(function(a){a.fn.konami=function(b,c){c=a.extend({},a.fn.konami.params,c);this.each(function(){var d=a(this);d.bind("konami",b).bind("keyup",function(e){a.fn.konami.checkCode(e,c,d);});});return this;};a.fn.konami.params={code:[38,38,40,40,37,39,37,39,66,65],step:0};a.fn.konami.checkCode=function(b,c,d){if(b.keyCode==c.code[c.step]){c.step++;}else{c.step=0;}if(c.step==c.code.length){d.trigger("konami");c.step=0;}};})(jQuery);
var piechart_options = {type: 'pie', width: '17px', height: '17px', sliceColors: ["#0f0", "#f00", "#999", "#ff0"] };

jQuery(document).ready(function($) {
  
  $('textarea.editor').each(function(index) {
    CodeMirror.fromTextArea(this, {
      lineNumbers: true,
      matchBrackets: true
    });
  });
  
  $('a.node_details').click(function(e) {
    $('#details').load($(this).attr('href'));
    $('tr.selected').removeClass('selected');
    $(this).parents('tr').addClass('selected');
    e.preventDefault();
  });
  
  $('#details .roles a').live('click', function(e) {
    link = $(this);
    $.getJSON(link.attr('href'), function(data) {
      $('a.highlight').removeClass('highlight');
      link.addClass('highlight');
      $('tr.selected').removeClass('selected');
      $.each(data['nodes'], function(i,node) {
        $('tr#'+node).addClass('selected');
      });
    });
    e.preventDefault();
  });
  
  $('#details .barclamps a').live('click', function(e) {
    link = $(this);
    $.getJSON(link.attr('href'), function(data) {
      $('a.highlight').removeClass('highlight');
      link.addClass('highlight');
      $('tr.selected').removeClass('selected');
      $.each(data['nodes'], function(i,node) {
        $('tr#'+node).addClass('selected');
      });
    });
    e.preventDefault();
  });
  
  $('.inline_piechart').sparkline('html', piechart_options );
  
  setInterval( function() {
    $('.led.unready, .led.pending').toggleClass('blink');
  }, 500);
  
  if(typeof update == 'function') { 
    setInterval("update()", 10000);
  }
  
  $('.button').live('click', function() {
    var button = $(this);
    button.addClass('pressed');
    if(button.attr('data-remote')=='true') {
      button.bind('ajax:complete', function(){ button.removeClass('pressed'); });
    }
  });
  
  $('input[data-default]').each(function() {
    $(this).val($(this).attr('data-default')).addClass('default');
  }).click(function(e){
    $(this).val('').removeClass('default');
  });
  
  $(document).konami(function(){
    $("header h1 a").css('background-image','url("/images/layout/bunny.png")').css('width','279px');
  });
});
