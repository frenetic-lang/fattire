Set Implicit Arguments.

Require Import Coq.Structures.Equalities.

Require Import Word.WordInterface.

Axiom bytes : Type.

Extract Constant bytes => "Cstruct.buf".

Definition portId := Word16.t.
Definition dlAddr := Word48.t.
Definition dlTyp := Word16.t.
Definition dlVlan := Word16.t.
Definition dlVlanPcp := Word8.t. (* 3 bits *)
Definition nwAddr := Word32.t.
Definition nwProto := Word8.t.
Definition nwTos := Word8.t. (** 6 bits *)
Definition tpPort := Word16.t.

Record of_match : Type := Match {
  matchDlSrc : option dlAddr;
  matchDlDst : option dlAddr;
  matchDlTyp : option dlTyp;
  matchDlVlan : option dlVlan;
  matchDlVlanPcp : option dlVlanPcp;
  matchNwSrc : option nwAddr;
  matchNwDst : option nwAddr;
  matchNwProto : option nwProto;
  matchNwTos : option nwTos;
  matchTpSrc : option tpPort;
  matchTpDst : option tpPort;
  matchInPort : option portId
}.

Record capabilities : Type := Capabilities {
  flow_stats: bool;   
  table_stats: bool;
  port_stats: bool;    
  stp: bool;
  ip_reasm: bool;
  queue_stats: bool;    
  arp_match_ip: bool
}.

Record actions : Type := Actions {
  output: bool;
  set_vlan_id: bool;
  set_vlan_pcp: bool;
  strip_vlan: bool;
  set_dl_src: bool;
  set_dl_dst: bool;
  set_nw_src: bool;
  set_nw_dst: bool;
  set_nw_tos: bool;
  set_tp_src: bool;
  set_tp_dst: bool;
  enqueue: bool;
  vendor: bool
}.

Record features : Type := Features {
  switch_id : Word64.t;
  num_buffers: Word32.t;
  num_tables: Word8.t;
  supported_capabilities: capabilities;
  supported_actions: actions
  (* TODO(arjun): physical ports go here  *)
}.

Inductive flowModCommand : Type :=
| AddFlow : flowModCommand
| ModFlow : flowModCommand
| ModStrictFlow : flowModCommand
| DeleteFlow : flowModCommand
| DeleteStrictFlow : flowModCommand.

Definition priority := Word16.t.
Definition cookie := Word32.t.
Definition bufferId := Word16.t.

Inductive pseudoPort : Type :=
| PhysicalPort : portId -> pseudoPort
| InPort : pseudoPort
| Flood : pseudoPort
| AllPorts : pseudoPort
| Controller : Word16.t -> pseudoPort.

Inductive action : Type :=
| Output : pseudoPort -> action
| SetDlVlan : dlVlan -> action
| SetDlVlanPcp : dlVlanPcp -> action
| StripVlan : action
| SetDlSrc : dlAddr -> action
| SetDlDst : dlAddr -> action
| SetNwSrc : nwAddr -> action
| SetNwDst : nwAddr -> action
| SetNwTos : nwTos -> action
| SetTpSrc : tpPort -> action	
| SetTpDst : tpPort -> action.

Definition actionSequence := list action.

Inductive timeout : Type :=
| Permanent : timeout
| ExpiresAfter : forall (n : Word16.t), 
    Word16.to_nat n > Word16.to_nat Word16.zero -> 
    timeout.

(* TODO(arjun): Missing the flag for emergency flows. *)
Record flowMod := FlowMod {
  mfModCmd : flowModCommand;
  mfMatch : of_match;
  mfPriority : priority;
  mfActions : actionSequence;
  mfCookie : cookie;
  mfIdleTimeOut : timeout;
  mfHardTimeOut : timeout; 
  mfNotifyWhenRemoved : bool;
  mfApplyToPacket : option bufferId;
  mfOutPort : option pseudoPort;
  mfOverlapAllowed : bool
}.

Definition xid : Type := Word32.t.

Inductive message : Type :=
| Hello : bytes -> message
| EchoRequest : bytes -> message
| EchoReply : bytes -> message
| FeaturesRequest : message
| FeaturesReply : features -> message
| FlowModMsg : flowMod -> message.


(*
Inductive OnlyFromSwitchMessage : Type :=
| BarrierReply : OnlyFromSwitchMessage
| PacketIn : option BufferID -> Port -> Packet -> OnlyFromSwitchMessage.

Inductive OnlyFromControllerMessage : Type :=
| ModifyFlow : ModFlowRecord -> OnlyFromControllerMessage
| BarrierRequest : OnlyFromControllerMessage
| PacketOut : (BufferID + Packet) -> 
    option Port -> 
    ActionSequence -> 
    OnlyFromControllerMessage.

Definition FromSwitchMessage := 
  (SymmetricMessage + OnlyFromSwitchMessage) % type.

Definition FromControllerMessage := 
  (SymmetricMessage + OnlyFromControllerMessage) % type.
*)