let PExpr : Type = < PLit : Text | PResourceId : Text >

let ServerSideEncryption : Type = < AES256 | AWSKMS >

let Param : Type =
  { paramName  : Text
  , paramValue : Text
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
  { PExpr = PExpr
  , ServerSideEncryption = ServerSideEncryption
  , Param = Param
  , Stack = Stack
  , DhallConfig = DhallConfig
  }