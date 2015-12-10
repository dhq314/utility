/**
 * 常用的工具函数
 * Require: Bootstrap Style
 */
var Util = {

    /**
     * 添加时间单位
     * @param time
     * @returns {string}
     */
    trans_up_time: function (time) {
        var second = parseInt(time / 1000);
        var minute = 0;
        var hour = 0;
        if (second > 60) {
            minute = parseInt(second / 60);
            second = parseInt(second % 60);
            if (minute > 60) {
                hour = parseInt(minute / 60);
                minute = parseInt(minute % 60);
            }
        }
        var result = "" + parseInt(second) + "秒";
        if (minute > 0) {
            result = "" + parseInt(minute) + "分" + result;
        }
        if (hour > 0) {
            result = "" + parseInt(hour) + "小时" + result;
        }
        return result;
    },

    /**
     * 添加字节大小单位
     * @param Byte
     * @returns {string}
     */
    trans_size: function (Byte) {
        var KB = Byte / 1024;
        var MB = KB / 1024;
        var GB = MB / 1024;
        if (GB > 10) {
            return Util.float2unit(GB, 100) + " GB";
        } else if (MB > 10) {
            return Util.float2unit(MB, 100) + " MB";
        } else if (KB > 0) {
            return Util.float2unit(KB, 100) + " KB";
        } else {
            return Byte + " B";
        }
    },

    /**
     * 保留小数点位数
     * @param float
     * @param digit
     * @returns {number}
     */
    float2unit: function (float, digit) {
        return parseInt(float * digit) / digit;
    },

    show_tips: function (msg, interval, cls) {
        var tvo = $("#util-tips-div");
        if (tvo.length > 0) {
            tvo.html(msg);
        } else {
            cls = cls == "" ? "alert-warning" : cls;
            var _html = [];
            _html.push('<div id="util-tips-div" class="alert ', cls, '" style="position: absolute; left: 48%; top: 0; padding: 8px 15px;">');
            _html.push(msg);
            _html.push('</div>');
            $(".container").eq(0).append(_html.join(""));
            if (interval > 0) {
                setTimeout(function () {
                    Util.hide_tips();
                }, 3000)
            }
        }
    },

    hide_tips: function () {
        $('#util-tips-div').remove();
    }

};

