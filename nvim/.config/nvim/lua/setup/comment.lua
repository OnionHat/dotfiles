local ok_status, comment = pcall(require, "Comment")
if not ok_status then
    return
end

comment.setup()
