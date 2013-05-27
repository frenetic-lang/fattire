open NetworkPacket
open WordInterface

val coq_VLAN_NONE : dlVlan

type 'a mask = { m_value : 'a; m_mask : 'a option }

val m_value : 'a1 mask -> 'a1

val m_mask : 'a1 mask -> 'a1 option

type xid = Word32.t

val val_to_mask : 'a1 -> 'a1 mask

type switchId = Word64.t

type groupId = Word32.t

type portId = Word32.t

type tableId = Word8.t

type bufferId = Word32.t

type oxm =
| OxmInPort of portId
| OxmInPhyPort of portId
| OxmMetadata of Word64.t mask
| OxmEthType of Word16.t
| OxmEthDst of Word48.t mask
| OxmEthSrc of Word48.t mask
| OxmVlanVId of Word12.t mask
| OxmVlanPcp of Word8.t
| OxmIPProto of Word8.t
| OxmIPDscp of Word8.t
| OxmIPEcn of Word8.t
| OxmIP4Src of Word32.t mask
| OxmIP4Dst of Word32.t mask
| OxmTCPSrc of Word16.t mask
| OxmTCPDst of Word16.t mask
| OxmARPOp of Word16.t
| OxmARPSpa of Word32.t mask
| OxmARPTpa of Word32.t mask
| OxmARPSha of Word48.t mask
| OxmARPTha of Word48.t mask
| OxmICMPType of Word8.t
| OxmICMPCode of Word8.t
| OxmMPLSLabel of Word32.t
| OxmMPLSTc of Word8.t
| OxmTunnelId of Word64.t mask

type oxmMatch = oxm list

type pseudoPort =
| PhysicalPort of portId
| InPort
| Flood
| AllPorts
| Controller of Word16.t
| Any

type action =
| Output of pseudoPort
| Group of groupId
| PopVlan
| PushVlan
| PopMpls
| PushMpls
| SetField of oxm

type actionSequence = action list

type instruction =
| GotoTable of tableId
| ApplyActions of actionSequence
| WriteActions of actionSequence

type bucket = { bu_weight : Word16.t; bu_watch_port : portId option;
                bu_watch_group : groupId option; bu_actions : actionSequence }

val bu_weight : bucket -> Word16.t

val bu_watch_port : bucket -> portId option

val bu_watch_group : bucket -> groupId option

val bu_actions : bucket -> actionSequence

type groupType =
| All
| Select
| Indirect
| FF

type groupMod =
| AddGroup of groupType * groupId * bucket list
| DeleteGroup of groupType * groupId

type timeout =
| Permanent
| ExpiresAfter of Word16.t

type flowModCommand =
| AddFlow
| ModFlow
| ModStrictFlow
| DeleteFlow
| DeleteStrictFlow

type flowModFlags = { fmf_send_flow_rem : bool; fmf_check_overlap : bool;
                      fmf_reset_counts : bool; fmf_no_pkt_counts : bool;
                      fmf_no_byt_counts : bool }

val fmf_send_flow_rem : flowModFlags -> bool

val fmf_check_overlap : flowModFlags -> bool

val fmf_reset_counts : flowModFlags -> bool

val fmf_no_pkt_counts : flowModFlags -> bool

val fmf_no_byt_counts : flowModFlags -> bool

type flowMod = { mfCookie : Word64.t mask; mfTable_id : tableId;
                 mfCommand : flowModCommand; mfIdle_timeout : timeout;
                 mfHard_timeout : timeout; mfPriority : Word16.t;
                 mfBuffer_id : bufferId option;
                 mfOut_port : pseudoPort option;
                 mfOut_group : groupId option; mfFlags : flowModFlags;
                 mfOfp_match : oxmMatch; mfInstructions : instruction list }

val mfCookie : flowMod -> Word64.t mask

val mfTable_id : flowMod -> tableId

val mfCommand : flowMod -> flowModCommand

val mfIdle_timeout : flowMod -> timeout

val mfHard_timeout : flowMod -> timeout

val mfPriority : flowMod -> Word16.t

val mfBuffer_id : flowMod -> bufferId option

val mfOut_port : flowMod -> pseudoPort option

val mfOut_group : flowMod -> groupId option

val mfFlags : flowMod -> flowModFlags

val mfOfp_match : flowMod -> oxmMatch

val mfInstructions : flowMod -> instruction list

type packetInReason =
| NoMatch
| ExplicitSend

type packetIn = { pi_buffer_id : Word32.t option; pi_total_len : Word16.t;
                  pi_reason : packetInReason; pi_table_id : tableId;
                  pi_cookie : Word64.t; pi_ofp_match : oxmMatch;
                  pi_pkt : packet option }

val pi_buffer_id : packetIn -> Word32.t option

val pi_total_len : packetIn -> Word16.t

val pi_reason : packetIn -> packetInReason

val pi_table_id : packetIn -> tableId

val pi_cookie : packetIn -> Word64.t

val pi_ofp_match : packetIn -> oxmMatch

val pi_pkt : packetIn -> packet option

type capabilities = { flow_stats : bool; table_stats : bool;
                      port_stats : bool; group_stats : bool; ip_reasm : 
                      bool; queue_stats : bool; port_blocked : bool }

type portState = { link_down : bool; blocked : bool; live : bool }

type portDesc = { port_no : portId; state : portState }

type portReason =
  | PortAdd
  | PortDelete
  | PortModify

type portStatus = { reason : portReason; desc : portDesc }

val flow_stats : capabilities -> bool

val table_stats : capabilities -> bool

val port_stats : capabilities -> bool

val group_stats : capabilities -> bool

val ip_reasm : capabilities -> bool

val queue_stats : capabilities -> bool

val port_blocked : capabilities -> bool

type features = { datapath_id : Word64.t; num_buffers : Word32.t;
                  num_tables : Word8.t; aux_id : Word8.t;
                  supported_capabilities : capabilities }

val datapath_id : features -> Word64.t

val num_buffers : features -> Word32.t

val num_tables : features -> Word8.t

val aux_id : features -> Word8.t

val supported_capabilities : features -> capabilities

type packetOut = { po_buffer_id : bufferId option; po_in_port : pseudoPort;
                   po_actions : actionSequence; po_pkt : packet option }

val po_buffer_id : packetOut -> bufferId option

val po_in_port : packetOut -> pseudoPort

val po_actions : packetOut -> actionSequence

val po_pkt : packetOut -> packet option

type message =
| Hello
| EchoRequest of bytes
| EchoReply of bytes
| FeaturesRequest
| FeaturesReply of features
| FlowModMsg of flowMod
| GroupModMsg of groupMod
| PacketInMsg of packetIn
| PacketOutMsg of packetOut
| PortStatusMsg of portStatus
| BarrierRequest
| BarrierReply
