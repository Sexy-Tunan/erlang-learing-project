程序交互1：
(1) 写一个程序代码 work_a.er，实现每隔2秒钟输出日志(比如写“我正在工作 1”,“我正在工作2”..)到一个文件 manager.log，
    然后 30 秒后写一句话"success"到另一个文件 success.log(表示work a已经完成任务)。最后退出程序。

(2) 写一个程序代码 tail_b.en。实现监控日志的功能:实时追踪日志，同时先显示最后 10 行历史内容。类似 linux 命令: tail -n 10 -fmanager.log.
    先打印这个 manager.log 文件的最后 10 行的日志，然后当 manager.log 文件有新内容时，则很快显示出来。 
    有当检测到有 success.log 文件(表示 work a已经完成任务)后，就删掉success.log，并且退出程序。

(3) 先执行 work a，10 秒后 再执行 tail b
(4) 启动两个 er进程，实现交互。

程序交互2：
    功能同上, 但是要改变成两个不同的节点之间的消息通讯
    可以用 net adm 模块或者rpc 模块 实现两个节点的消息通讯。