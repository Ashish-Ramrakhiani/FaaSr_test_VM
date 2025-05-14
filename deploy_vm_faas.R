deploy_vm_faas <- function(resource_intensive_functions) {
  # Access .faasr directly to get VM configuration and InvocationID
  faasr_log("Starting VM deployment for resource-intensive functions")
  
  # Verify we have InvocationID
  if (is.null(.faasr$InvocationID) || .faasr$InvocationID == "") {
    err_msg <- "Missing InvocationID in payload"
    faasr_log(err_msg)
    stop(err_msg)
  }
  
  faasr_log(paste0("Using InvocationID: ", .faasr$InvocationID))
  
  # Find VM server configuration
  vm_server_name <- NULL
  for (server_name in names(.faasr$ComputeServers)) {
    if (.faasr$ComputeServers[[server_name]]$FaaSType == "VM") {
      vm_server_name <- server_name
      break
    }
  }
  
  if (is.null(vm_server_name)) {
    err_msg <- "No VM server defined in configuration"
    faasr_log(err_msg)
    stop(err_msg)
  }
  
  # Get instance details from VM configuration
  instance_id <- .faasr$ComputeServers[[vm_server_name]]$InstanceId
  region <- .faasr$ComputeServers[[vm_server_name]]$Region
  access_key <- .faasr$ComputeServers[[vm_server_name]]$AccessKey
  secret_key <- .faasr$ComputeServers[[vm_server_name]]$SecretKey
  
  faasr_log(paste0("Starting EC2 instance: ", instance_id, " in region: ", region))
  
  # Create EC2 client
  ec2 <- paws.compute::ec2(
    config = list(
      credentials = list(
        creds = list(
          access_key_id = access_key,
          secret_access_key = secret_key
        )
      ),
      region = region
    )
  )
  
  # Check if instance is already running
  describe_result <- ec2$describe_instances(
    InstanceIds = list(instance_id)
  )
  
  instance_state <- describe_result$Reservations[[1]]$Instances[[1]]$State$Name
  
  if (instance_state != "running") {
    start_result <- ec2$start_instances(
      InstanceIds = list(instance_id)
    )
    faasr_log("Instance start command issued")
  } else {
    faasr_log("Instance is already running")
  }
  
  # Get instance details
  public_dns <- NULL
  max_attempts <- 60
  attempts <- 0
  
  while (is.null(public_dns) && attempts < max_attempts) {
    Sys.sleep(5)
    attempts <- attempts + 1
    
    describe_result <- ec2$describe_instances(
      InstanceIds = list(instance_id)
    )
    
    instance_state <- describe_result$Reservations[[1]]$Instances[[1]]$State$Name
    
    if (instance_state == "running") {
      public_dns <- describe_result$Reservations[[1]]$Instances[[1]]$PublicDnsName
      if (!is.null(public_dns) && public_dns != "") {
        faasr_log(paste0("Instance is running with DNS: ", public_dns))
        break
      }
    }
    
    faasr_log(paste0("Waiting for instance to be ready... (", attempts, "/", max_attempts, ")"))
  }
  
  if (is.null(public_dns) || public_dns == "") {
    err_msg <- "Failed to get public DNS for instance"
    faasr_log(err_msg)
    stop(err_msg)
  }
  
  # Find OpenWhisk API key by looking for OpenWhisk servers
  auth_key <- NULL
  for (server_name in names(.faasr$ComputeServers)) {
    if (.faasr$ComputeServers[[server_name]]$FaaSType == "OpenWhisk") {
      auth_key <- .faasr$ComputeServers[[server_name]]$API.key
      faasr_log(paste0("Using OpenWhisk API key from: ", server_name))
      break
    }
  }
  
  if (is.null(auth_key)) {
    auth_key <- "23bc46b1-71f6-4ed5-8c54-816aa4f8c502:123zO3xZCLrMN6v2BKK1dXYFpXlPkccOFqm12CdAsMgRU4VrNZ9lyGVCGuMDGIwP"
    faasr_log("Warning: No OpenWhisk server found, using default auth key")
  }
  
  # Create VM info
  vm_info <- list(
    instance_id = instance_id,
    region = region,
    public_dns = public_dns,
    endpoint = paste0("http://", public_dns, ":3233"),
    openwhisk = list(
      auth_key = auth_key,
      api_host = paste0("http://", public_dns, ":3233")
    ),
    status = "starting"
  )
  
  # Wait for OpenWhisk to be ready
  faasr_log("Waiting for OpenWhisk to be ready...")
  
  max_attempts <- 30
  attempt <- 1
  ready <- FALSE
  
  while (!ready && attempt <= max_attempts) {
    tryCatch({
      response <- httr::GET(
        url = paste0(vm_info$endpoint, "/api/v1/namespaces/guest/actions"),
        httr::add_headers(
          'Authorization' = paste0("Basic ", base64enc::base64encode(charToRaw(auth_key)))
        )
      )
      
      if (httr::status_code(response) == 200) {
        ready <- TRUE
        faasr_log("OpenWhisk is ready!")
      } else {
        faasr_log(paste0("OpenWhisk not ready yet. Status code: ", httr::status_code(response)))
        Sys.sleep(10)
      }
    }, error = function(e) {
      faasr_log(paste0("OpenWhisk not ready yet. Error: ", e$message))
      Sys.sleep(10)
    })
    
    attempt <- attempt + 1
  }
  
  # Update VM status
  vm_info$status <- "ready"
  
  # Save VM info to S3 using InvocationID from .faasr
  vm_info_path <- paste0(.faasr$FaaSrLog, "/", .faasr$InvocationID, "/vm_info.json")
  faasr_log(paste0("Storing VM info at: ", vm_info_path))
  
  writeLines(jsonlite::toJSON(vm_info, auto_unbox = TRUE), "vm_info.json")
  faasr_put_file(local_file = "vm_info.json", 
                 remote_folder = .faasr$FaaSrLog, 
                 remote_file = paste0(.faasr$InvocationID, "/vm_info.json"))
  
  faasr_log("VM deployment completed successfully")
  return(list(status = "success", vm_info = vm_info))
}