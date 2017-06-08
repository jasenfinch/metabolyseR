
setClass('analysisParameters',
         slots = list(
           preTreat = 'list',
           classification = 'list',
           featureSelection = 'list',
           correlations = 'list'
         ))

setClass('Analysis',
         slots = list(
           log = 'list',
           parameters = 'analysisParameters',
           rawData = 'list',
           preTreated = 'list',
           classification = 'list',
           featureSelection = 'list',
           correlations = 'list'
         )
)