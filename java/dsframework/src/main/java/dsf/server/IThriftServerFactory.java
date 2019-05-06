/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package dsf.server;

import org.apache.thrift.server.TServer;

/**
 *
 * @author Administrator
 */
public interface IThriftServerFactory {
    TServer create() throws Throwable;
}
