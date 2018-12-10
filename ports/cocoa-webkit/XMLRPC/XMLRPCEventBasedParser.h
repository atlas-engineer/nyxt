#import <Foundation/Foundation.h>

@class XMLRPCEventBasedParserDelegate;

@interface XMLRPCEventBasedParser : NSObject<NSXMLParserDelegate> {
    NSXMLParser *myParser;
    XMLRPCEventBasedParserDelegate *myParserDelegate;
    BOOL isFault;
}

- (id)initWithData: (NSData *)data;

- (id)parse;

- (void)abortParsing;

- (NSError *)parserError;

- (BOOL)isFault;

@end
