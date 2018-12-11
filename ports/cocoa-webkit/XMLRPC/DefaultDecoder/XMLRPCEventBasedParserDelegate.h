#import <Foundation/Foundation.h>
#import "XMLRPCDecoder.h"
#import "XMLRPCElementParser.h"

@interface XMLRPCEventBasedParserDelegate : NSObject<NSXMLParserDelegate> {
    // Without ARC this reference is effectively unretained so don't use strong reference here.
    XMLRPCEventBasedParserDelegate * __unsafe_unretained myParent;
    NSMutableSet *myChildren;
    XMLRPCElementType myElementType;
    NSString *myElementKey;
    id myElementValue;
}

- (id)initWithParent: (XMLRPCEventBasedParserDelegate *)parent;

#pragma mark -

- (void)setParent: (XMLRPCEventBasedParserDelegate *)parent;

- (XMLRPCEventBasedParserDelegate *)parent;

#pragma mark -

- (void)setElementType: (XMLRPCElementType)elementType;

- (XMLRPCElementType)elementType;

#pragma mark -

- (void)setElementKey: (NSString *)elementKey;

- (NSString *)elementKey;

#pragma mark -

- (void)setElementValue: (id)elementValue;

- (id)elementValue;

@end
