
setClass('Analysis',
         slots = list(
           log = 'list',
           parameters = 'list',
           rawData = 'list',
           preTreated = 'list',
           classification = 'list',
           featureSelection = 'data.frame',
           correlations = 'list'
         )
)