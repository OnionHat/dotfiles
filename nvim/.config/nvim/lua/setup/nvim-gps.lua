local ok_status, gps = pcall(require, "nvim-gps")
if not ok_status then
    return
end
gps.setup()
