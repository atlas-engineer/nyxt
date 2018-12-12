//
// Copyright (C) 2012 Eric Czarny <eczarny@gmail.com>
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Foundation/Foundation.h>

@class XMLRPCEventBasedParserDelegate;

@interface XMLRPCDefaultResponseDecoder : NSObject <NSXMLParserDelegate> {
    NSXMLParser* myParser;
    XMLRPCEventBasedParserDelegate* myParserDelegate;
    BOOL isFault;
}

- (id)initWithData:(NSData*)data;

- (id)parse;

- (void)abortParsing;

- (NSError*)parserError;

- (BOOL)isFault;

@end
