terminate_vm_faas <- function() {
  # Access .faasr directly for InvocationID
  faasr_log("Starting VM termination")
  
  # Verify we have InvocationID
  if (is.null(.faasr$InvocationID) || .faasr$InvocationID == "") {
    err_msg <- "Missing InvocationID in payload"
    faasr_log(err_msg)
    stop(err_msg)
  }
  
  faasr_log(paste0("Using InvocationID: ", .faasr$InvocationID))
  
  # Get VM info from S3 using InvocationID from .faasr
  vm_info_path <- paste0(.faasr$FaaSrLog, "/", .faasr$InvocationID, "/vm_info.json")
  faasr_log(paste0("Retrieving VM info from: ", vm_info_path))
  
  tryCatch({
    faasr_get_file(remote_folder = .faasr$FaaSrLog, 
                   remote_file = paste0(.faasr$InvocationID, "/vm_info.json"), 
                   local_file = "vm_info.json")
    
    vm_info <- jsonlite::fromJSON("vm_info.json")
    
    # Find the VM server with matching instance ID
    vm_server_name <- NULL
    for (server_name in names(.faasr$ComputeServers)) {
      if (.faasr$ComputeServers[[server_name]]$FaaSType == "VM" &&
          .faasr$ComputeServers[[server_name]]$InstanceId == vm_info$instance_id) {
        vm_server_name <- server_name
        break
      }
    }
    
    if (is.null(vm_server_name)) {
      faasr_log("Warning: No matching VM server found in configuration, using first VM server")
      for (server_name in names(.faasr$ComputeServers)) {
        if (.faasr$ComputeServers[[server_name]]$FaaSType == "VM") {
          vm_server_name <- server_name
          break
        }
      }
    }
    
    if (is.null(vm_server_name)) {
      err_msg <- "No VM server defined in configuration"
      faasr_log(err_msg)
      stop(err_msg)
    }
    
    # Get credentials from .faasr
    access_key <- .faasr$ComputeServers[[vm_server_name]]$AccessKey
    secret_key <- .faasr$ComputeServers[[vm_server_name]]$SecretKey
    
    # Create EC2 client
    ec2 <- paws.compute::ec2(
      config = list(
        credentials = list(
          creds = list(
            access_key_id = access_key,
            secret_access_key = secret_key
          )
        ),
        region = vm_info$region
      )
    )
    
    # Stop the instance
    faasr_log(paste0("Stopping EC2 instance: ", vm_info$instance_id))
    
    stop_result <- ec2$stop_instances(
      InstanceIds = list(vm_info$instance_id)
    )
    
    faasr_log("EC2 instance stop command issued")
    
    # Wait for instance to stop
    max_attempts <- 30
    attempt <- 1
    stopped <- FALSE
    
    while (!stopped && attempt <= max_attempts) {
      Sys.sleep(10)
      
      describe_result <- ec2$describe_instances(
        InstanceIds = list(vm_info$instance_id)
      )
      
      instance_state <- describe_result$Reservations[[1]]$Instances[[1]]$State$Name
      
      faasr_log(paste0("Instance state: ", instance_state, " (attempt ", attempt, "/", max_attempts, ")"))
      
      if (instance_state == "stopped" || instance_state == "stopping") {
        stopped <- TRUE
        faasr_log("Instance is stopping/stopped")
        break
      }
      
      attempt <- attempt + 1
    }
    
    # Update VM info in S3
    vm_info$status <- "stopped"
    writeLines(jsonlite::toJSON(vm_info, auto_unbox = TRUE), "vm_info_stopped.json")
    faasr_put_file(local_file = "vm_info_stopped.json", 
                   remote_folder = .faasr$FaaSrLog, 
                   remote_file = paste0(.faasr$InvocationID, "/vm_info_stopped.json"))
    
    faasr_log("VM termination completed successfully")
    return(list(status = "success"))
    
  }, error = function(e) {
    err_msg <- paste0("Error terminating VM: ", e$message)
    faasr_log(err_msg)
    stop(err_msg)
  })
}