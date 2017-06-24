
setClass('AnalysisParameters',
         slots = list(
           preTreat = 'list',
           classification = 'list',
           featureSelection = 'list',
           correlations = 'list'
         ))

setClass('Analysis',
         slots = list(
           log = 'list',
           parameters = 'AnalysisParameters',
           rawData = 'list',
           preTreated = 'list',
           classification = 'list',
           featureSelection = 'list',
           correlations = 'list'
         )
)