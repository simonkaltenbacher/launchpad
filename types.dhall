let Protocol : Type = < HttpsProtocol | S3Protocol >

let PExpr : Type = < PLit : Text | PResourceId : { protocol : Protocol, resourceId : Text } >

let lit = \(value : Text) -> PExpr.PLit value

let https = \(resourceId : Text) ->
  PExpr.PResourceId
    { protocol = Protocol.HttpsProtocol
    , resourceId = resourceId
    }

let s3 = \(resourceId : Text) ->
  PExpr.PResourceId
    { protocol = Protocol.S3Protocol
    , resourceId = resourceId
    }

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
  { https                = https
  , lit                  = lit
  , Param                = Param
  , PExpr                = PExpr
  , Protocol             = Protocol
  , s3                   = s3
  , ServerSideEncryption = ServerSideEncryption
  , Stack                = Stack
  , DhallConfig          = DhallConfig
  }