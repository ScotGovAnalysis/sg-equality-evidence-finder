//google analytics
var policyEquality = "grid";//custom variable for analytics
var currentPage = "home";//custom variable for analytics
var lastNav = "";
window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}

//enable back button navigation by setting the browser page state to the Evidence Finder page ref
//shiny apps are single webpages, so the EEF "page" is kept track of manually in the policyEquality JS variable and the browser pop state
//the EEF "pages" are grid (the home page) and the equality/theme intersection pages
window.onpopstate = function(event) {
  if(event.state === null) {
    gtag('config', 'GA_MEASUREMENT_ID', {'page_path': '/'});
    policyEquality = "grid";
    currentPage = "home";
    lastNav = "";
    $('a[data-value=\"equality\"]')[0].click();
  } else if(event.state.equality === '' && event.state.policy === '') {
    gtag('config', 'GA_MEASUREMENT_ID', {'page_path': '/'});
    policyEquality = "grid";
    currentPage = "home";
    lastNav = event.state.navLink;
    $('a[data-value=\"equality\"]')[0].click();
  } else {
    gtag('config', 'GA_MEASUREMENT_ID', {'page_path': '/'+event.state.policy+'_'+event.state.equality});
    policyEquality = event.state.policy+'_'+event.state.equality; //google analytics
    currentPage = event.state.policy;
    lastNav = "";
    $('a[data-pol=\"'+event.state.policy+'\"][data-char=\"'+event.state.equality+'\"]')[0].click();
  }
};
   

//close help boxes with escape
$(document).keyup(function(e){if(e.keyCode === 27) {
  $('.eef-help,#eef-help-blank,#eef-help-close').hide();
}});

$(document).ready(function(){
  //help boxes
  $('#eef-help-close a,#eef-help-blank').click(function(){
    $('.eef-help,#eef-help-blank,#eef-help-close').hide();
  });
  
  //home page header link
  $('a[data-value=\"equality\"]').click(function(){
    if(policyEquality != 'grid') { //if event state hasn't been updated, then go to the home page
      window.history.pushState({equality: '', policy: '', navLink: ''}, null, null);
      lastNav = "";
   }
    policyEquality = "grid"; //google analytics
    currentPage = "home";
    Shiny.onInputChange('policyID','');
    Shiny.onInputChange('equalityID','');
    $('.eef:not(.eef-main)').hide();
    $('.eef-main').show();
    $('.eef-section-header-expand').show();
    $('.eef-section-content,.eef-section-header-collapse').hide();
    $('.eef-equality-buttons9.eef-summ.inactive').removeClass('inactive');
    $('.eef-equality-buttons9:not(.eef-summ):not(.inactive)').addClass('inactive');
    $('.eef-section.eef-main').each(function(index,value){
      var section = $(this).attr('id');
      Shiny.onInputChange(section+'EqualityID','overview');
      $('.'+section+'-buttons').removeClass('selected');
      $('#'+section+'-overview').addClass('selected');
     });
     if(lastNav !== '') {
       $(lastNav)[0].scrollIntoView();
       $(lastNav).focus();
    } else {
      scroll(0,0);
    }
  });

  //header links 
  $('#equality-menu2').click(function(){
    if(policyEquality != 'grid') {
      window.history.pushState({equality: '', policy: '', navLink: ''}, null, null);
    }
    policyEquality = "grid"; //google analytics
    currentPage = "home";
    Shiny.onInputChange('policyID','');
    Shiny.onInputChange('equalityID','');
    $('.eef:not(.eef-main)').hide();
    $('.eef-main').show();
    $('.eef-section-header-expand').show();
    $('.eef-section-content,.eef-section-header-collapse').hide();
    $('.eef-equality-buttons9.eef-summ.inactive').removeClass('inactive');
    $('.eef-equality-buttons9:not(.eef-summ):not(.inactive)').addClass('inactive');
    $('#equality-npf-top')[0].scrollIntoView();
    $('#equality-npf-top').focus();
    $('.eef-section.eef-main').each(function(index,value){
      var section = $(this).attr('id');
      Shiny.onInputChange(section+'EqualityID','overview');
      $('.'+section+'-buttons').removeClass('selected');
      $('#'+section+'-overview').addClass('selected');
    });
  });
  $('#equality-menu3').click(function(){
    $('#equality-publications-top')[0].scrollIntoView();
    $('#equality-publications-top').focus();
  });
  $('#equality-menu4').click(function(){
    $('#equality-contact-top')[0].scrollIntoView();
    $('#equality-contact-top').focus();
  });
  $('#equality-menu5').click(function(){
    $('#eef-help-blank,#eef-help-close,#eef-help-navigation').show().trigger('shown').removeClass('shinyjs-hide');
    $('#eef-help-navigation-top').focus();
  });
    $('#equality-menu6').click(function(){
      var policy = $(this).attr('data-pol');
      var equality = $(this).attr('data-char');
      if(policyEquality!=policy+'_'+equality) {
       window.history.pushState({equality: equality, policy: policy, navLink: ''}, null, null);
        gtag('config', 'GA_MEASUREMENT_ID', {'page_path': '/'+policy+'_'+equality});
      }
      policyEquality = policy+'_'+equality; //google analytics
      currentPage = policy;
      Shiny.onInputChange('policyID',policy);
      Shiny.onInputChange('equalityID',equality);
       
      $('.eef,.eef-main').hide();//hide all elements with the "eef" class
    $('.shinyjs-hide.eef-'+policy).removeClass('shinyjs-hide'); //show all elements with the "eef-<policyArea>" class
    $('.eef-'+policy).show().trigger('shown'); //show all elements with the "eef-<policyArea>" class
    scroll(0,0);
    $('#equality-menu1').focus();
  });

  //grid navigation
  //navigation to different "pages" is handled by using the classes "eef" and "eef-<policyId>"
  //when clicking on the grid all elements with the "eef" class are hidden and all elements with
  //the "eef-<policyID>" class are shown. Any element with the class "eef eef-<policyID>" is treated as being on the specified policy page (note that some elements are tagged to appear on multiple "pages")
  $('.eef-grid-square,.eef-grid-summ,.eef-mobile-grid.sub').click(function(){
    var policy = $(this).attr('data-pol');
    var equality = $(this).attr('data-char');
    var navLink = '.'+$(this).attr('class').split(/\s+/)[0]+'[data-pol=\"'+policy+'\"]'+'[data-char=\"'+equality+'\"]';
    if(policyEquality!=policy+'_'+equality) {
      window.history.replaceState({equality: '', policy: '', navLink: navLink}, null, null);
      window.history.pushState({equality: equality, policy: policy, navLink: ''}, null, null);
      gtag('config', 'GA_MEASUREMENT_ID', {'page_path': '/'+policy+'_'+equality});
   }
    policyEquality = policy+'_'+equality; //google analytics
    currentPage = policy;
    
    scroll(0,0);
    //$('#equality-menu1').focus();
    $('.eef,.eef-main').hide();//hide all elements with the "eef" class
    $('.shinyjs-hide.eef-'+policy).removeClass('shinyjs-hide'); //show all elements with the "eef-<policyArea>" class
    $('.eef-'+policy).show().trigger('shown'); //show all elements with the "eef-<policyArea>" class
    $('.eef-'+policy+' > .eef-section-header')[0].focus(); //focus on first topic section
    $('.eef-section-header-expand').show(); //show all elements with the "eef-<policyArea>" class
    $('.eef-section-content,.eef-section-header-collapse').hide(); //hide content that's in drop down toggle boxes
    $('.eef-equality-buttons9.eef-'+policy+'.inactive').removeClass('inactive');
    $('.eef-equality-buttons9:not(.eef-'+policy+'):not(.inactive)').addClass('inactive');
    $('.eef-section.eef-'+policy+'.eef-'+equality+' .eef-section-header:not(.active)').addClass('active');
    $('.eef-section.eef-'+policy+':not(.eef-'+equality+') .eef-section-header.active').removeClass('active');
    if(Shiny.onInputChange === undefined) {
      setTimeout(function () {
                  Shiny.onInputChange('policyID',policy);
                  Shiny.onInputChange('equalityID',equality);
                  $('.eef-section.eef-'+policy).each(function(index,value){
                    Shiny.onInputChange($(this).attr('id')+'EqualityID',equality);
                  });
                }, 10); 
    } else {
      Shiny.onInputChange('policyID',policy);
      Shiny.onInputChange('equalityID',equality);
    }
    $('.eef-section.eef-'+policy).each(function(index,value){
      var section = $(this).attr('id');
      if(Shiny.onInputChange !== undefined) {Shiny.onInputChange(section+'EqualityID',equality);}
      $('.'+section+'-buttons').removeClass('selected');
      $('#'+section+'-'+equality).addClass('selected');
     });
  });
  
 //mobile grid navigation
 $('.eef-grid-summ,.eef-mobile-grid.sub[data-pol=\"summ\"]').click(function(){
    var equality = $(this).attr('data-char');
    $('.shinyjs-hide.eef-summ-'+equality).removeClass('shinyjs-hide');
    $('.eef-summ-'+equality).show().trigger('shown');
  });

 //expandable headers 
 $('.eef-section-header,.eef-mobile-grid.main').click(function(){
  $(this).next().slideToggle(500).removeClass('shinyjs-hide').trigger('shown');
  $(this).children('.eef-section-header-expand,.eef-section-header-collapse').toggle().removeClass('shinyjs-hide').trigger('shown');
  });
  

  //section buttons
  $('.eef-equality-buttons8,.eef-equality-buttons9').click(function(){
    var section = $(this).attr('data-id');
    var sectionEquality = $(this).attr('data-char');
    Shiny.onInputChange(section+'EqualityID',sectionEquality);
    $('.'+section+'-buttons').removeClass('selected');
    $('#'+section+'-'+sectionEquality).addClass('selected');
    gtag('event', 'toggleSection', {'event_category': currentPage+'_'+sectionEquality, 'event_label': $(this).attr('data-section') });
  });
  
  //no longer needed?
  $('#dashboard a[data-toggle=\"tab\"]').click(function(){scroll(0,0);policyEquality="grid";currentPage="home";});

  //custom EEF google analytics code to add events to track what users click on
  //gridNav is the EEF "page"
  $('.eef-grid-square,.eef-grid-summ').click(function(){
    gtag('event', 'gridNav', {'event_category': policyEquality, 'event_label': policyEquality });
  });
          
  //toggleSection is the section boxes that users click to view
  $('.eef-section-header').click(function(){
    gtag('event', 'toggleSection', {'event_category': policyEquality, 'event_label': $(this).attr('data-section')});
  });
        
  //click is an event for miscellaneous clicks
  $('.eef-page-links2').click(function(){
    gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });
  });

  //footer link to scroll to top and return focus to the top (focus needs to be set for keyboard navigability)
  $('.eef-footer-top').click(function(){
	scroll(0,0);
	$('#equality-menu1').focus();
  });

  
});
