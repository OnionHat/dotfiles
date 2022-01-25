local ok_status, comment = pcall(require, "Comment")
if not ok_status then
	return
end

comment.setup()

local ft = require("Comment.ft")

ft.set("python", { "#%s", '"""%s\n"""' })
