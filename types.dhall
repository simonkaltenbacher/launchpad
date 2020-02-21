let Protocol : Type = < HttpsProtocol | S3Protocol >

let PExpr : Type = < PLit : Text | PResourceId : { protocol : Protocol, resourceId : Text } >

let ServerSideEncryption : Type = < AES256 | AWSKMS >

let Param : Type =
  { paramName  : Text
  , paramValue : PExpr
  }

let Stack : Type =
  { roleArn         : Optional Text
  , stackName       : Text 
  , stackTemplateId : Text
  , stackParams     : List Param
  }

let DhallConfig : Type =
  { resourceBucketName   : Text
  , sseKmsKeyId          : Optional Text
  , serverSideEncryption : Optional ServerSideEncryption
  , stacks               : List Stack
  }

in
  { Param = Param
  , PExpr = PExpr
  , Protocol = Protocol
  , ServerSideEncryption = ServerSideEncryption
  , Stack = Stack
  , DhallConfig = DhallConfig
  }