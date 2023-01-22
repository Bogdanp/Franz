#lang lua

local is_null = racket["null?"]
local car, cdr = racket.car, racket.cdr
local string_to_bytes = racket["string->bytes/utf-8"]
local parse_internal = _ENV["#%parse-Internal"]
local is_internal_offset_commit = _ENV["#%InternalOffsetCommit?"]
local is_internal_group_metadata = _ENV["#%InternalGroupMetadata?"]
local internal_offset_commit_group = _ENV["#%InternalOffsetCommit-group"]
local internal_offset_commit_topic = _ENV["#%InternalOffsetCommit-topic"]
local internal_offset_commit_partition_id = _ENV["#%InternalOffsetCommit-partition-id"]
local internal_offset_commit_offset = _ENV["#%InternalOffsetCommit-offset"]
local internal_group_metadata_group = _ENV["#%InternalGroupMetadata-group"]
local internal_group_metadata_generation = _ENV["#%InternalGroupMetadata-generation"]
local internal_group_metadata_protocol_type = _ENV["#%InternalGroupMetadata-protocol-type"]
local internal_group_metadata_protocol_data = _ENV["#%InternalGroupMetadata-protocol-data"]
local internal_group_metadata_leader = _ENV["#%InternalGroupMetadata-leader"]
local internal_group_metadata_members = _ENV["#%InternalGroupMetadata-members"]
local internal_group_member_id = _ENV["#%InternalGroupMember-id"]
local internal_group_member_client_id = _ENV["#%InternalGroupMember-client-id"]
local internal_group_member_client_host = _ENV["#%InternalGroupMember-client-host"]
local internal_group_member_rebalance_timeout = _ENV["#%InternalGroupMember-rebalance-timeout"]
local internal_group_member_session_timeout = _ENV["#%InternalGroupMember-session-timeout"]
local internal_group_member_subscription = _ENV["#%InternalGroupMember-subscription"]
local internal_group_member_assignment = _ENV["#%InternalGroupMember-assignment"]

local OffsetCommit = {}
OffsetCommit.__name = "OffsetCommit"
function OffsetCommit.new(impl)
    local ob = {
        group = string_to_bytes(internal_offset_commit_group(impl)),
        topic = string_to_bytes(internal_offset_commit_topic(impl)),
        partition_id = internal_offset_commit_partition_id(impl),
        offset = internal_offset_commit_offset(impl)
    }
    setmetatable(ob, OffsetCommit)
    return ob
end

function OffsetCommit:__tostring()
    return string.format(
        "<OffsetCommit group=%q topic=%q partition_id=%d offset=%d>",
        self.group, self.topic, self.partition_id, self.offset
    )
end

local function safe_string_to_bytes(maybe_s)
    if maybe_s then
        return string_to_bytes(maybe_s)
    end
    return nil
end

local GroupMetadata = {}
GroupMetadata.__name = "GroupMetadata"
function GroupMetadata.new(impl)
    local members = {}
    local lst = internal_group_metadata_members(impl)
    local idx = 1
    while not is_null(lst) do
        local member = car(lst)
        members[idx] = {
            id = string_to_bytes(internal_group_member_id(member)),
            client_id = string_to_bytes(internal_group_member_client_id(member)),
            client_host = string_to_bytes(internal_group_member_client_host(member)),
            rebalance_timeout = internal_group_member_rebalance_timeout(member) or nil,
            session_timeout = internal_group_member_session_timeout(member),
            subscription = internal_group_member_subscription(member) or nil,
            assignment = internal_group_member_assignment(member) or nil
        }
        idx = idx + 1
        lst = cdr(lst)
    end

    local ob = {
        group = string_to_bytes(internal_group_metadata_group(impl)),
        generation = internal_group_metadata_generation(impl),
        protocol_type = safe_string_to_bytes(internal_group_metadata_protocol_type(impl)),
        protocol_data = safe_string_to_bytes(internal_group_metadata_protocol_data(impl)),
        leader = safe_string_to_bytes(internal_group_metadata_leader(impl)),
        members = members
    }
    setmetatable(ob, GroupMetadata)
    return ob
end

function GroupMetadata:__tostring()
    return string.format("<GroupMetadata group=%q members=%d>", self.group, #self.members)
end

local kafka = {
    OffsetCommit = OffsetCommit,
    GroupMetadata = GroupMetadata
}

function kafka.parse_committed_offset(record)
    if not record.key then
        return nil
    end
    local internal = parse_internal(record.key, record.value or "")
    if is_internal_offset_commit(internal) then
        return "offset_commit", OffsetCommit.new(internal)
    elseif is_internal_group_metadata(internal) then
        return "group_metadata", GroupMetadata.new(internal)
    else
        return nil
    end
end

function kafka.record_size(record)
    local size = 0
    if record.key then
        size = size + #record.key
    end
    if record.value then
        size = size + #record.value
    end
    if record.headers then
        for k, v in pairs(record.headers) do
            size = size + #k
            if v then
                size = size + #v
            end
        end
    end
    return size
end

return kafka
