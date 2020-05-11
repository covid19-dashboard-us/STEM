save.files = function(result.state, result.county = NULL, file.path) {
  
  # state -------------------------------------------------------------------------------------------
  
  names(result.state$P.state.lower.infec) = c('State',paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
  write.table(file = paste0(file.path, '/Predict_forward_',date.pred[1],'_state_lower.tsv'),
              result.state$P.state.lower.infec, quote=FALSE,sep="\t",row.names=FALSE)
  names(result.state$P.state.upper.infec) = c('State',paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
  write.table(file = paste0(file.path, '/Predict_forward_',date.pred[1],'_state_upper.tsv'),
              result.state$P.state.upper.infec, quote=FALSE,sep="\t",row.names=FALSE)
  names(result.state$P.state.pred.infec) = c('State',paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
  write.table(file = paste0(file.path, '/Predict_forward_',date.pred[1],'_state.tsv'),
              result.state$P.state.pred.infec, quote=FALSE,sep="\t",row.names=FALSE)
  
  names(result.state$P.state.lower.new.infec) = c('State',paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
  write.table(file = paste0(file.path, '/PredictNew_forward_',date.pred[1],'_state_lower.tsv'),
              result.state$P.state.lower.new.infec, quote=FALSE,sep="\t",row.names=FALSE)
  names(result.state$P.state.upper.new.infec) = c('State',paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
  write.table(file = paste0(file.path, '/PredictNew_forward_',date.pred[1],'_state_upper.tsv'),
              result.state$P.state.upper.new.infec, quote=FALSE,sep="\t",row.names=FALSE)
  names(result.state$P.state.pred.new.infec) = c('State',paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
  write.table(file = paste0(file.path, '/PredictNew_forward_',date.pred[1],'_state.tsv'),
              result.state$P.state.pred.new.infec, quote=FALSE,sep="\t",row.names=FALSE)
  
  names(result.state$P.state.lower.death) = c('State',paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
  write.table(file = paste0(file.path, '/Predict_forward_',date.pred[1],'_state_lower_death.tsv'),
              result.state$P.state.lower.death, quote=FALSE,sep="\t",row.names=FALSE)
  names(result.state$P.state.upper.death) = c('State',paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
  write.table(file = paste0(file.path, '/Predict_forward_',date.pred[1],'_state_upper_death.tsv'),
              result.state$P.state.upper.death, quote=FALSE,sep="\t",row.names=FALSE)
  names(result.state$P.state.pred.death) = c('State',paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
  write.table(file = paste0(file.path, '/Predict_forward_',date.pred[1],'_state_death.tsv'),
              result.state$P.state.pred.death, quote=FALSE,sep="\t",row.names=FALSE)
  
  names(result.state$P.state.lower.new.death) = c('State',paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
  write.table(file = paste0(file.path, '/PredictNew_forward_',date.pred[1],'_state_lower_death.tsv'),
              result.state$P.state.lower.new.death, quote=FALSE,sep="\t",row.names=FALSE)
  names(result.state$P.state.upper.new.death) = c('State',paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
  write.table(file = paste0(file.path, '/PredictNew_forward_',date.pred[1],'_state_upper_death.tsv'),
              result.state$P.state.upper.new.death, quote=FALSE,sep="\t",row.names=FALSE)
  names(result.state$P.state.pred.new.death) = c('State',paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
  write.table(file = paste0(file.path, '/PredictNew_forward_',date.pred[1],'_state_death.tsv'),
              result.state$P.state.pred.new.death, quote=FALSE,sep="\t",row.names=FALSE)
  
  
  # county -------------------------------------------------------------------------------------------
  if(!is.null(result.county)) {
    names(result.county$P.county.lower.infec) = c('ID',"State" ,"County" ,paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
    write.table(file = paste0(file.path, '/Predict_forward_',date.pred[1],'_county_lower.tsv'),
                result.county$P.county.lower.infec, quote=FALSE,sep="\t",row.names=FALSE)
    names(result.county$P.county.upper.infec) = c('ID',"State" ,"County" ,paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
    write.table(file = paste0(file.path, '/Predict_forward_',date.pred[1],'_county_upper.tsv'),
                result.county$P.county.upper.infec, quote=FALSE,sep="\t",row.names=FALSE)
    names(result.county$P.county.pred.infec) = c('ID',"State" ,"County" ,paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
    write.table(file = paste0(file.path, '/Predict_forward_',date.pred[1],'_county.tsv'),
                result.county$P.county.pred.infec, quote=FALSE,sep="\t",row.names=FALSE)
    
    names(result.county$P.county.lower.new.infec) = c('ID',"State" ,"County" ,paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
    write.table(file = paste0(file.path, '/PredictNew_forward_',date.pred[1],'_county_lower.tsv'),
                result.county$P.county.lower.new.infec, quote=FALSE,sep="\t",row.names=FALSE)
    names(result.county$P.county.upper.new.infec) = c('ID',"State" ,"County" ,paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
    write.table(file = paste0(file.path, '/PredictNew_forward_',date.pred[1],'_county_upper.tsv'),
                result.county$P.county.upper.new.infec, quote=FALSE,sep="\t",row.names=FALSE)
    names(result.county$P.county.pred.new.infec) = c('ID',"State" ,"County" ,paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
    write.table(file = paste0(file.path, '/PredictNew_forward_',date.pred[1],'_county.tsv'),
                result.county$P.county.pred.new.infec, quote=FALSE,sep="\t",row.names=FALSE)
    
    names(result.county$P.county.lower.death) = c('ID',"State" ,"County" ,paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
    write.table(file = paste0(file.path, '/Predict_forward_',date.pred[1],'_county_lower_death.tsv'),
                result.county$P.county.lower.death, quote=FALSE,sep="\t",row.names=FALSE)
    names(result.county$P.county.upper.death) = c('ID',"State" ,"County" ,paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
    write.table(file = paste0(file.path, '/Predict_forward_',date.pred[1],'_county_upper_death.tsv'),
                result.county$P.county.upper.death, quote=FALSE,sep="\t",row.names=FALSE)
    names(result.county$P.county.pred.death) = c('ID',"State" ,"County" ,paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
    write.table(file = paste0(file.path, '/Predict_forward_',date.pred[1],'_county_death.tsv'),
                result.county$P.county.pred.death, quote=FALSE,sep="\t",row.names=FALSE)
    
    names(result.county$P.county.lower.new.death) = c('ID',"State" ,"County" ,paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
    write.table(file = paste0(file.path, '/PredictNew_forward_',date.pred[1],'_county_lower_death.tsv'),
                result.county$P.county.lower.new.death, quote=FALSE,sep="\t",row.names=FALSE)
    names(result.county$P.county.upper.new.death) = c('ID',"State" ,"County" ,paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
    write.table(file = paste0(file.path, '/PredictNew_forward_',date.pred[1],'_county_upper_death.tsv'),
                result.county$P.county.upper.new.death, quote=FALSE,sep="\t",row.names=FALSE)
    names(result.county$P.county.pred.new.death) = c('ID',"State" ,"County" ,paste0('X', as.character(date.pred, format = '%Y.%m.%d')))
    write.table(file = paste0(file.path, '/PredictNew_forward_',date.pred[1],'_county_death.tsv'),
                result.county$P.county.pred.new.death, quote=FALSE,sep="\t",row.names=FALSE)
  }
}