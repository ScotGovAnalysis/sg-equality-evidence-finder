//cookie control config
var config = {
  apiKey: '5ffa3f1866786dee76275ef7cb58b9a9c239a133',//scotland.shinyapps.io
  //apiKey: 'b494c0eaad7ec39e8532ff3901b70480c05dc049',//test
  product: 'COMMUNITY',
  initialState: 'OPEN',
  statement : {
    description: 'For more information vist our',
    name : 'Privacy Statement',
    url: 'privacyStatement.html',
    updated : '26/11/2018'
  },
  
  optionalCookies: [
    {
      name: 'analytics',
      label: 'Analytics',
      description: 'Analytical cookies help us to improve our website by collecting and reporting information on its usage.',
      recommendedState: true,
      cookies: ['_ga', '_gid', '_gat', '__utma', '__utmt', '__utmb', '__utmc', '__utmz', '__utmv'],
      onAccept : function(){
        
        //google analytics code snippet (gtag version)
        window.dataLayer = window.dataLayer || [];//also copied to EEF.js to allow using gtag elsewhere
        function gtag(){dataLayer.push(arguments);}//also copied to EEF.js to allow using gtag 
        gtag('js', new Date());
        gtag('config', 'UA-128676670-1', { 'anonymize_ip':true });//EEF account
        //gtag('config', 'UA-128846881-1', { 'anonymize_ip':true });//test
        
        //custom EEF google analytics code to add events
        $(document).ready(function(){
          $('.eef-grid-square,.eef-grid-summ').click(function(){
            gtag('event', 'gridNav', {'event_category': policyEquality, 'event_label': policyEquality });
          });
          
          $('.eef-section-header').click(function(){
            gtag('event', 'toggleSection', {'event_category': policyEquality, 'event_label': $(this).attr('data-section')});
          });
        
          $('.eef-page-links2,.eef-equality-buttons8,.eef-equality-buttons9').click(function(){
          gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });
          });
        });
         
      },
      onRevoke: function(){
        
        //turn off google analytics code for EEF
        window['ga-disable-UA-128676670-1'] = true;//EEF account
        //window['ga-disable-UA-128846881-1'] = true;//test
      }
    }/*,{
                    name: 'preferences',
                    label: 'Preferences',
                    description: '',
                    recommendedState: true,
                    cookies: [],
                    onAccept : function(){},
                    onRevoke: function(){}
                }*/
  ],
  position: 'RIGHT',
  theme: 'DARK'
};
    
CookieControl.load( config );
  
  
  
  
