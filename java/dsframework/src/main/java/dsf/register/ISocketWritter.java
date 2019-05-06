/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package dsf.register;

import dsf.core.Message;

/**
 *
 * @author Administrator
 */
public interface ISocketWritter {

    void send(Message msg) throws Exception;

    void send(String name, Object value) throws Exception;
    
}
