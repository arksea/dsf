var del_handler = function() {
    if(confirm('确定要删除服务吗?')){
        return true;
    } else {
        return false;
    }
}
document.getElementById("btn_delete").onclick = del_handler;